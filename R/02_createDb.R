# Make permanent db ------------------------------------------------------------
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")

# List files containing obs.
files_obs <- list.files(wd$obs, full.names = T)
# list files containing ids.
files_ids <- list.files(wd$ids, full.names = T)

# Find naming overlaps.
names_obs <- names( data.table::fread(files_obs[1]) )
names_ids <- names( data.table::fread(files_ids[1]) )


# Save all observations to database. -------------------------------------------
nrowsUploaded <- 0
for(i in files_obs) {
  name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(i))
  print(paste("working on", name, "which is", which(files_obs==i), "of", length(files_obs)))
  dt <- data.table::fread(i)
  # Rename columns with "obs_" prefix to avoid name overlap with id tables.
  dt <- dplyr::rename_at(dt, vars(names_obs[ names_obs %in% names_ids ]), ~paste0("obs_", .x) )
  # Separate date/time entries.
  dt <- tidyr::separate(dt, col = obs_created_at, into = paste0("obs_created_at_", c("year", "month", "day", "hour", "minute", "second")))
  # Rename ID columns.
  dt <- dplyr::rename(dt, obs_ID = obs_id, obs_taxonID = taxon.id )
  # Select distinct.
  dt <- dplyr::distinct(dt)
  # Write.
  if( which(files_obs==i) == 1 ) {
    dbWriteTable(iNat, "obs0", dt, overwrite = T)
  } else {
    dbAppendTable(iNat, "obs0", dt)
  }
  # Tally rows
  nrowsUploaded <- nrowsUploaded + nrow(dt)
}
dbListTables(iNat)
print(paste0("Uploaded ", nrowsUploaded, " rows."))
#89255106

## Filter to unique obs_IDs only ----
dbSendQuery(
  iNat, statement = 
    "
    CREATE TABLE IF NOT EXISTS `obs` AS
      SELECT DISTINCT * FROM
        (SELECT `obs_ID`, `observed_on`, `obs_taxonID`, `obs_created_at_year`, `obs_created_at_month`, `obs_created_at_day`, `obs_created_at_hour`, `obs_created_at_minute`, `obs_created_at_second`, `quality_grade`, 
          MAX(`num_identification_agreements`) AS `num_identification_agreements`, 
          MAX(`num_identification_disagreements`) AS `num_identification_disagreements`, `obs_taxon.rank`, `taxon.rank_level` AS `obs_taxon.rank_level`, `latitude`, `longitude`, `obs_user.id`, `obs_user.login`
        from obs0
        GROUP BY `obs_ID`)"
  )
dbListTables(iNat)
dbRemoveTable(iNat, "obs0")
dbListTables(iNat)

dbSendQuery(iNat, "CREATE INDEX obs_ID on obs(obs_ID);")                         # Make index by obs_ID
dbSendQuery(iNat, "CREATE INDEX lonlat on obs(longitude, latitude);")            # Make index by coordinates
dbSendQuery(iNat, "CREATE INDEX observerID on obs(`obs_user.id`);")
dbGetQuery(iNat, "PRAGMA index_list(obs);")                                     # Check indexes on table.


# Save all identifications to database -----------------------------------------
# To determine if ID is made by observing user, we rely on the obs table 
# generated in the last step.
obs <- tbl(iNat, "obs")
obsDeets <- dplyr::select(obs, obs_ID, obs_user.id)
# Import each file one at a time, perform some tidying + data prep.
for(i in files_ids) {
  name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(i))
  print(paste("working on", name, "which is", which(files_ids==i), "of", length(files_ids)))
  dt <- data.table::fread(i)
  # Rename columns with "ids_" prefix to avoid name overlap with id tables.
  dt <- dplyr::rename_at(dt, vars(names_ids[ names_ids %in% names_obs ]), ~paste0("id_", .x) )
  # Separate date/time entries.
  dt <- tidyr::separate(dt, col = id_created_at, into = paste0("id_created_at_", c("year", "month", "day", "hour", "minute", "second")))
  # Rename ID columns.
  dt <- dplyr::rename(dt, id_ID = id_id, obs_ID = inat_id  , ids_taxonID = taxon_id)
  # Determine if identification made by observing user
  myids <- left_join(dt, obsDeets, by = "obs_ID" ,copy = T) %>% 
    dplyr::mutate(is_observer = id_user.id == obs_user.id) %>% 
    dplyr::select(-obs_user.id)
  # Select distinct
  myids <- dplyr::distinct(myids)
  
  # Write.
  if( which(files_ids==i) == 1 ) {
    dbWriteTable(iNat, "ids0", myids, overwrite = T)
  } else {
    dbAppendTable(iNat, "ids0", myids)
  }
}

dbSendQuery(
  iNat, 
  statement = paste0( 
    "CREATE TABLE IF NOT EXISTS `ids` AS
      SELECT DISTINCT * FROM
        (SELECT `id_ID`, `ids_taxonID`, `id_user.id`, `id_created_at_year`, `id_created_at_month`, `id_created_at_day`, `id_created_at_hour`, `id_created_at_minute`, `id_created_at_second`, `category`,`id_taxon.rank`, `disagreement`, `vision`, `previous_observation_taxon_id`, `taxon.is_active`, `obs_ID`, `is_observer`, MAX(`id_user.login`) AS `id_user.login`
        FROM ids0 GROUP BY `id_ID`
      )
  ")
)
dbListTables(iNat)
dbRemoveTable(iNat, "ids0")
dbListTables(iNat) 
 
dbSendQuery(iNat, "CREATE INDEX obsID_IDs on ids(obs_ID, id_ID);")
dbSendQuery(iNat, "CREATE INDEX IDerID on ids(`id_user.id`);")
dbGetQuery(iNat, "PRAGMA index_list(ids);")
  
# Check output -----------------------------------------------------------------  

dbListTables(iNat)
tbl(iNat, dbListTables(iNat)[1])
tbl(iNat, dbListTables(iNat)[2])

# Disconnect -------------------------------------------------------------------s
dbDisconnect(iNat)
