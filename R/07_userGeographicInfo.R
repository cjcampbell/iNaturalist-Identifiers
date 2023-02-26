
# Setup ------------------------------------------------------------------------

library(sf)
library(sp)
library(adehabitatHR)

iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")
dbListTables(iNat)


# Also calculate area of average identifications --------------------------
getBox <- function(coords, percent = 99, id) {
  margin <- (100-percent)/200
  mybox <- coords %>% 
    sf_project(from =  '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',to = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs') %>% 
    as.data.frame() %>% 
    pivot_longer(cols = 1:2) %>% 
    dplyr::group_by(name) %>% 
    dplyr::summarise(
      qlow = quantile(value, margin, na.rm = T), 
      qhigh = quantile(value, 1-margin, na.rm = T)
      ) %>% 
    dplyr::select(-name) %>% 
    t %>% 
    as.data.frame %>% 
    sf::st_as_sf(crs = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs', coords = c(1,2)) %>% 
    st_bbox()
  bdeets <- mybox %>% as.matrix %>% t %>% as.data.frame 
  out <- data.frame(id = id, area = st_area(st_as_sfc(mybox)), bdeets)
  return(out)
}


# Find geographic distribution of identifications by user ----------------------
cat("Find geographic distribution of identifications by user")

activeTables <- dbListTables(iNat)
if(!"idsLocationsByUser" %in% activeTables) {
  dbSendQuery(
    iNat,
    paste0(
      "
    CREATE TABLE idsLocationsByUser AS
      SELECT `user.id`, `id_ID`,`latitude`, `longitude`  FROM
        (SELECT * FROM idsByUser WHERE n_obs_IDed >= 100) AS user
      LEFT JOIN
        (SELECT `id_ID`, `id_user.id`, `obs_ID`, `is_observer` FROM ids WHERE `is_observer` = 0) AS ids
      ON user.`user.id` = ids.`id_user.id`
      LEFT JOIN
        (SELECT `obs_ID`, `longitude`, `latitude` FROM obs WHERE NOT (`longitude` IS NULL) AND NOT (`latitude` IS NULL)) AS obs
      ON ids.`obs_ID` = obs.`obs_ID`
    "
    )
  )
  dbSendQuery(iNat, "CREATE INDEX locUserID on idsLocationsByUser(`user.id`);")
  dbListTables(iNat)
}


binpath <- file.path(wd$bin, "userIDgeo")
if(!dir.exists(binpath)) dir.create(binpath)

nn <- dbGetQuery(iNat,  "SELECT COUNT(DISTINCT(`user.id`)) FROM idsLocationsByUser" ) %>% unlist
by <- 5000
offset <- seq(0, nn, by = by)

ptm <- Sys.time()
for(i in offset) {
  writeLines(paste0("\n", "Batch number ", which(offset == i), " of ", length(offset), "."))
  cat("Running query to get IDs for batch of users."); cat(Sys.time() - ptm)
  
  coordDeets <- dbGetQuery(
    iNat,
    paste0(
      "
      SELECT ids.`user.id` AS `user.id`, `id_ID`,`latitude`,`longitude` FROM
       (SELECT DISTINCT `user.id` FROM idsLocationsByUser LIMIT ", by, " OFFSET ", i, ") AS ids
      LEFT JOIN
        idsLocationsByUser AS deets
      ON ids.`user.id` = deets.`user.id`
      "
    )
  )
  if(nrow(coordDeets) == 0) cat("No rows in this batch"); next
  
  cat("Getting box describing observer patterns by users"); cat(Sys.time() - ptm)
  mdf <- bettermc::mclapply(unique(coordDeets$user.id), mc.cores = 20, function(id) {
    #print(id)
    set.seed(42)
    coords <- coordDeets[coordDeets$user.id == id, ] %>%
      na.omit() %>% 
      distinct()
    if(nrow(coords) > 5000) {
      coords <- sample_n(coords, 5000, replace = FALSE)
    }
    coords <- dplyr::select(coords, longitude, latitude)
    spdf <- getBox(coords, id = id)
    return(spdf)
  }) %>% 
    bind_rows()

  ## Find distance ----
  cat('Find distance'); cat(Sys.time() - ptm)
  obsDeets <- dbGetQuery(
    iNat,
    paste0(
      "SELECT user.`user.id` AS `user.id`, `obs_ID`, `latitude`,`longitude` FROM
        (SELECT DISTINCT `user.id` FROM idsLocationsByUser LIMIT ", by, " OFFSET ", i, ") AS user
      LEFT JOIN
        (SELECT `obs_user.id`, `obs_ID`, `latitude`, `longitude` FROM obs) AS obs
      ON user.`user.id` = obs.`obs_user.id`
    ")
  )
  
  cents <- obsDeets %>% 
    group_by(user.id) %>% 
    dplyr::summarise(
      cen_lon = mean(longitude, na.rm = T),
      cen_lat = mean(latitude, na.rm = T)
    )
  
  ## Find mean and sd of distance ----
  cat('Find mean and sd of distance'); cat(Sys.time() - ptm)
  dd <- left_join(coordDeets, cents, by = "user.id") %>% 
    na.omit %>% 
    # filter out any weird cases where centroid doesn't have valid coords.
    dplyr::filter(
      cen_lon > -180, cen_lon < 180, cen_lat > -180, cen_lat < 180,
      longitude > -180, longitude < 180, latitude > -180, latitude < 180) 
  
  out2 <-
    geosphere::distGeo(p1 = dd[,c("cen_lon", "cen_lat")], p2 = dd[,c("longitude", "latitude")]) %>% 
    data.frame(dd, dist_m = .) %>% 
    group_by(user.id) %>% 
    dplyr::summarise(
      mean_dist = mean(dist_m, na.rm = T),
      sd_dist   = sd(dist_m, na.rm = T)
      )
  mdf <- left_join(mdf, out2, by = c("id"="user.id"))
  
  ## Ascertain overlap... ----
  cat('Ascertain overlap'); cat(Sys.time() - ptm)
  mdf <- cents[,2:3] %>% 
    sf_project(from =  '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',to = '+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs') %>% 
    data.frame(cents[,1], .) %>% 
    left_join(mdf, by = c("user.id" = "id")) %>% 
    dplyr::mutate(
      overlaps = case_when(
        X1 >= xmin & X1 <= xmax & X2 >= ymin & X2 <= ymax ~ 1, 
        TRUE ~ 0
      )
    ) %>% 
    dplyr::rename(
      cent_lon = X1, cent_lat = X2
    )
  cat("Writing"); cat(Sys.time() - ptm)
  # Write.
  if( i  == 0 ) {
    dbWriteTable(iNat, "ider_area", mdf, overwrite = T)
  } else {
    dbAppendTable(iNat, "ider_area", mdf)
  }
  print(Sys.time() - ptm)
}


# Explore (optional) -------------------------------------------------------
ider_area <- dbGetQuery(
  iNat,
  paste0(
    "
    SELECT area.`user.id` AS `user.id`, `cent_lon`, `cent_lat`, `area`, `xmin`, `ymin`, `xmax`, `ymax`, `mean_dist`, `sd_dist`, `overlaps`, `n_IDs`, `n_obs_IDed`, `n_taxa_IDed`, `idsPerobs_IDed`, `n_obs`, `n_taxa_observed` FROM
      (SELECT * FROM ider_area) AS area
    LEFT JOIN
      (SELECT * FROM idsByUser WHERE n_obs_IDed >= 100) AS idsByUser
    ON area.`user.id` = idsByUser.`user.id`
    ")
)
fwrite(ider_area, file = file.path(wd$out, "ider_area.csv"))


# Glossary ----------------------------------------------------------------
# "user.id"      --   user ID number    
# "cent_lon"     --   longitude of centroid of users's observations     
# "cent_lat"     --   latitude of centroid of users's observations          
# "area"         --   area of bounding box encompassing 5k random IDs of each user (m2)
# "xmin"         --   coordinates of bounding box of 5k random IDs made by user 
# "ymin"         --   coordinates of bounding box of 5k random IDs made by user 
# "xmax"         --   coordinates of bounding box of 5k random IDs made by user 
# "ymax"         --   coordinates of bounding box of 5k random IDs made by user 
# "mean_dist"    --   distance in meters from centroid of user's observations to 5k random IDs made by user (mean)       
# "sd_dist"      --   distance in meters from centroid of user's observations to 5k random IDs made by user (standard deviation)       
# "overlaps"     --   does bounding box of 5k identifications encompass the user's observations centroid      


# Disconnect --------------------------------------------------------------

dbDisconnect(iNat)
cat("Done")
