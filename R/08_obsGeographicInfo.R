# Setup -------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(maptools)
library(DBI)
library(bettermc)
sf::sf_use_s2(FALSE)

## Country ----

print("Prepping country info per coord")
## Setup ----
# Unlink if needed for a rerun:
#if(dir.exists(file.path(wd$bin, "geoTMP"))) unlink(file.path(wd$bin, "geoTMP"), recursive = T)
if(!dir.exists(file.path(wd$bin, "geoTMP"))) dir.create(file.path(wd$bin, "geoTMP"))

data(wrld_simpl)

world <- wrld_simpl %>%
  st_as_sf(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  st_make_valid() %>%
  st_transform("+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs")

## Grid ----

# 1000km
mygrid <- st_make_grid(world, cellsize = 1500e3, square = F) %>%
  st_as_sf() %>%
  mutate(st_sf(geometry = .), grid_id = 1:nrow(.))

maxGridID <- mygrid$grid_id %>% max
# 336

## Biome. ----
biomes <- file.path(wd$data, "biome", "regions", "tnc_terr_ecoregions.shp") %>% 
  st_read() %>% 
  st_as_sf() %>%
  st_transform("+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs")


## Extract country info for each coordinate in batches of 10000 ----

obs_lonlat <- fread(file.path(wd$bin, "obs_lonlat.csv")) %>% 
  dplyr::filter(!is.na(latitude), !is.na(longitude)) %>% 
  dplyr::mutate(coord_id = row_number())
myrows <- nrow(obs_lonlat)
x1 <- seq(1,myrows, by = 10000)
x2 <- c(x1[-1], myrows)

print("Beginning batching process")
ptm <- Sys.time()
bettermc::mclapply(1:length(x1), mc.cores = 5, function(i) {
  if(x1[i] < 1) stop("i must start at 1")
  if(x2[length(x2)] != myrows ) stop("last number must be nrow in obs_lonlat")
  df <- obs_lonlat[x1[i]:x2[i], ]

  suppressMessages({

    # load and convert coordinates -----
    obs_sf <- df %>%
      {sp::SpatialPointsDataFrame(
        data = ., coords = c(.[,1:2]),
        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) } %>%
      sf::st_as_sf() %>%
      st_make_valid() %>%
      st_transform("+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs")

    # Determine country where observation was taken. ----
    # For points directly over a country, determine country.
    obs_world_overlap <- st_join(obs_sf, world, join=st_intersects) %>%
      dplyr::select(ISO3, geometry, coord_id)

    # For points that do not overlap, find nearest features.
    noOverlap <- dplyr::filter(obs_world_overlap, is.na(ISO3))
    noOverlap_ISO3 <- noOverlap %>%
      st_nearest_feature(., world) %>%
      world$ISO3[.]
    obs_world_nearest <- select(noOverlap, coord_id, geometry) %>%
      dplyr::mutate(ISO3 = noOverlap_ISO3)

    # Unite
    ISO3_by_coord <-
      obs_world_overlap %>%
      dplyr::filter(!is.na(ISO3)) %>%
      bind_rows(obs_world_nearest) %>%
      dplyr::left_join(., df)
    ISO3_by_coord_dt <-
      ISO3_by_coord %>%
      data.table::as.data.table() %>%
      dplyr::select(ISO3, longitude, latitude)

    # Determine grid cell where observation was taken ------
    gridDeets <- st_join(obs_sf, mygrid, join=st_intersects) %>%
      as.data.frame() %>%
      dplyr::select(grid_id, nObs, longitude, latitude)

    # Determine biome ------
    biomeDeets <- st_join(obs_sf, biomes, join=st_intersects) %>%
      as.data.frame() %>%
      dplyr::select(ECO_NAME, WWF_MHTNAM, WWF_REALM2, nObs, longitude, latitude)
    
    
    # Join together. ---------
    out <- full_join(ISO3_by_coord_dt, gridDeets, by = c("longitude", "latitude")) %>% 
      full_join(biomeDeets, by = c("longitude", "latitude", "nObs"))

  })

  fwrite(out, file = file.path(wd$bin, "geoTMP", paste0(i, ".csv")))

})
print(Sys.time() - ptm)

## Get temp files and summarize them, then combine ----------

myfiles <- list.files( file.path(wd$bin, "geoTMP"), full.names = T)

r1 <- bettermc::mclapply(myfiles[1:length(myfiles)], mc.cores = 5, function(x){
  a1 <-fread(x) 
  if(!identical(names(a1), c("ISO3", "longitude", "latitude", "grid_id", "nObs", "ECO_NAME", 
                             "WWF_MHTNAM", "WWF_REALM2") )) {
    stop(x)
  }
  a1 %>% 
    dplyr::group_by(ISO3, grid_id, WWF_MHTNAM) %>% 
    dplyr::summarise(nObs = sum(nObs), .groups = "keep")
})


r2 <- r1 %>% 
  bind_rows() %>% 
  ungroup() %>% 
  dplyr::group_by(ISO3, grid_id, WWF_MHTNAM) %>% 
  dplyr::summarise(nObs = sum(nObs))
fwrite(r2, file = file.path(wd$out, "ObsGeoInfo.csv"))

print(Sys.time() - ptm)



# Get info for each lat/lon combo. ----------------------------------------
myfiles <- list.files( file.path(wd$bin, "geoTMP"), full.names = T)
r1 <- bettermc::mclapply(myfiles[1:length(myfiles)], mc.cores = 5, function(x){
  a1 <-fread(x) 
  a2 <- dplyr::select(a1, longitude, latitude, ISO3, grid_id, WWF_MHTNAM)
}) %>% bind_rows()

fwrite(r1, file = file.path(wd$out, "latLonGeoInfo.csv"))




# Add count data ----------------------------------------------------------
r2 <- r1 %>%
  left_join(., count(r1, ISO3, name = "nObs_ISO3")) %>% 
  left_join(., count(r1, WWF_MHTNAM, name = "nObs_biome")) %>% 
  left_join(., count(r1, grid_id, name = "nObs_gridID")) %>% 
  dplyr::rename(geo_biome = WWF_MHTNAM, geo_ISO3 = ISO3, geo_gridID = grid_id)

## Also save to database.
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "/srv/gspeedq32/mbelitz/iNatIDers/iNat.db")
dbWriteTable(iNat, "geoObs", r2, overwrite = T)

dbGetQuery(iNat, "CREATE INDEX obs_lonlat on geoObs(longitude, latitude);")    
dbGetQuery(iNat, "PRAGMA index_list(geoObs);")

# Close ----

dbDisconnect(iNat)
print(Sys.time() - ptm)
