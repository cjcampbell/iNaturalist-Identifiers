

iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")
dbListTables(iNat)

ids <- tbl(iNat, "ids")
obs <- tbl(iNat, "obs")


# QA / Info Gathering on IDs ----------------------------------

ids %>% dplyr::select(id_ID, obs_ID, id_created_at_year, id_created_at_month, disagreement, vision, everything())
set.seed(42)
sampleIDs <- dbGetQuery( iNat, 
  paste0(
    "SELECT * FROM ids
     ORDER BY RANDOM()
     LIMIT 10000"
  )
  )
sampleIDs2 <- dplyr::mutate(sampleIDs,
              link = paste0("https://www.inaturalist.org/identifications/", id_ID))
fwrite(sampleIDs2, file = file.path(wd$out, "QAQC", "10000randomIDs.csv"))

# Checks for Users --------------------------------------------------------

idsByUser <- fread( file.path(wd$out, "idsByUser.csv") )
set.seed(42)
idsByUser %>% 
  sample_n(10000) %>% 
  dplyr::select(user.id, n_IDs, n_obs) %>% 
  dplyr::mutate(link = paste0("https://www.inaturalist.org/people/", user.id)) %>% 
  fwrite(file.path(wd$out, "QAQC", "idsAndObsByUser.csv"))


