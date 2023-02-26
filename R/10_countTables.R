
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")
dbListTables(iNat)

ids <- tbl(iNat, "ids") %>% distinct()
obs <- tbl(iNat, "obs") %>% distinct()
# For each identification, determine whether it was made by the user that
# is also the observer of the observation in question.
ids_no_observer <-  dplyr::filter(ids, is_observer == TRUE )

# IDs and obs by user -----------------------------------------------------------------------

ids_df <- dbGetQuery(
  iNat, 
  paste0(
    "SELECT 
      `id_user.id` AS `user.id`,
      `obs_user.login` AS `user.login`,
      `n_IDs`,
      `n_obs_IDed`,
      `n_taxa_IDed`,
      `idsPerobs_IDed`,
      `n_obs`,
      `n_taxa_observed`
    FROM
        (
        SELECT 
          `id_user.id`,
          CAST(`n_IDs` AS FLOAT) / `n_obs_IDed` AS `idsPerobs_IDed`,
          `n_IDs`,
          `n_obs_IDed`,
          `n_taxa_IDed`
          FROM
          (
          SELECT 
            `id_user.id`,
            COUNT(DISTINCT `id_ID`) AS `n_obs_IDed`, 
            COUNT(*) AS `n_IDs`, 
            COUNT(DISTINCT(`ids_taxonID`)) AS `n_taxa_IDed`
          FROM 
            (SELECT DISTINCT * FROM ids)
          WHERE `is_observer` = 0
          GROUP BY `id_user.login`, `id_user.id`
          )
        ) AS LHS
      INNER JOIN
        (
        SELECT 
          COUNT(*) AS `n_obs`,
          COUNT(DISTINCT(`obs_taxonID`)) AS `n_taxa_observed`,
          `obs_user.login`, 
          `obs_user.id`
        FROM 
          (SELECT DISTINCT * FROM obs)
        GROUP BY `obs_user.login`, `obs_user.id`
        ) AS RHS
      ON LHS.`id_user.id` = RHS.`obs_user.id`
    "
  )
)
data.table::fwrite(ids_df, file = file.path(wd$out, "idsByUser.csv"))


# Proportion of each identifier's IDs at different rank levels -------------------------

meanRankLevelByUser <- dbGetQuery(iNat, 
        "SELECT AVG(`id_taxon.rank_level`) AS `meanRankLevel`,`id_user.id` FROM
           (SELECT `id_taxon.rank`, `ids_taxonID`, `id_user.id` FROM ids) AS ids
            LEFT JOIN taxonRankLevelInfo AS trli ON ids.`id_taxon.rank` = trli.`id_taxon.rank`
        GROUP BY `id_user.id`;")
fwrite(meanRankLevelByUser, file = file.path(wd$out, "meanRankLevelByUser.csv"))

# Cumulative counts by date ------------------------------------------------------------

## Count of observations -------------------------
x1 <- obs %>% 
  dplyr::group_by(obs_created_at_year, obs_created_at_month) %>% 
  dplyr::summarise(val = n())  %>% 
  ungroup %>% 
  dplyr::mutate(yr_precise = obs_created_at_year + obs_created_at_month/12) %>% 
  dplyr::arrange(yr_precise) %>% 
  dplyr::mutate(
    cum_val = cumsum(val),
    metric = "n_obs"
  ) 
date_obs <- collect(x1)

## Count of identifiers -------------------------

x2 <- ids_no_observer %>% 
  dplyr::filter(!is.na(id_created_at_year)) %>% 
  dplyr::group_by(id_created_at_year, id_created_at_month) %>% 
  dplyr::summarise(val = n_distinct(id_user.id) )  %>% 
  ungroup %>% 
  dplyr::mutate(yr_precise = id_created_at_year + id_created_at_month/12) %>% 
  dplyr::arrange(yr_precise) %>% 
  dplyr::mutate(
    cum_val = cumsum(val),
    metric = "n_IDers"
  ) %>% 
  show_query()
date_IDers <- collect(x2)

## Count of identifiers excluding observing identifier ----------
x2a <- ids_no_observer %>% 
  dplyr::filter(!is.na(id_created_at_year)) %>% 
  dplyr::group_by(id_created_at_year, id_created_at_month) %>% 
  dplyr::summarise(val = n_distinct(id_user.id) )  %>% 
  ungroup %>% 
  dplyr::mutate(yr_precise = id_created_at_year + id_created_at_month/12) %>% 
  dplyr::arrange(yr_precise) %>% 
  dplyr::mutate(
    cum_val = cumsum(val),
    metric = "n_IDers"
  ) %>% 
  show_query()
date_IDers_noObservers <- collect(x2a)

## Count of identifications -------------------
x3 <- ids_no_observer %>% 
  dplyr::filter(!is.na(id_created_at_year)) %>% 
  dplyr::group_by(id_created_at_year, id_created_at_month) %>% 
  dplyr::summarise(val = n() )  %>% 
  ungroup %>% 
  dplyr::mutate(yr_precise = id_created_at_year + id_created_at_month/12) %>% 
  dplyr::arrange(yr_precise) %>% 
  dplyr::mutate(
    cum_val = cumsum(val),
    metric = "n_IDs") %>% 
  show_query()
date_IDs <- collect(x3)

## Large count table --------------------------
# Format stuff
cumulativeEvents <- bind_rows(date_obs, date_IDers,date_IDs,date_IDers_noObservers)
data.table::fwrite(cumulativeEvents, file = file.path(wd$out, "cumulativeEvents.csv"))


# Counts by country ------------------------------------------------------------

coords <- tbl(iNat, "coords")

byCountry_simpl <- obs %>% 
  dplyr::select(latitude, longitude, obs_ID) %>% 
  right_join(., dplyr::select(ids_no_observer, c(id_ID, obs_ID)), by = c("obs_ID" = "obs_ID")) %>% 
  left_join( coords) %>% 
  right_join(., ids) %>% 
  group_by(ISO3) %>% 
  dplyr::summarise(
    obs_n = n_distinct(id_ID),
    ids_n = n_distinct(obs_ID),
    IDers_n = n_distinct(id_user.id)
  ) %>% 
  collect

byCountry_perObs <-  dplyr::select(obs, latitude, longitude, obs_ID) %>% 
  right_join(., dplyr::select(ids_no_observer, c(id_ID, obs_ID)), by = c("obs_ID" = "obs_ID")) %>% 
  left_join( coords) %>% 
  dplyr::group_by(ISO3, id_ID) %>% 
  dplyr::summarise(
    IDperObs = n_distinct(obs_ID)
  ) %>% 
  group_by(ISO3) %>% 
  dplyr::summarise(
    mean_IDperObs = mean( IDperObs  , na.rm = T),
    mode_IDperObs = mode( IDperObs ),
    med_IDperObs = median(IDperObs , na.rm = T)
  ) %>% 
  collect()

full_join(byCountry_simpl, byCountry_perObs) %>% 
  data.table::fwrite(., file = file.path(wd$out, "byCountry.csv"))


# Number of IDers by Taxa -----------------------------------------------------------

#### TODO add options excluding identifiers that are observer of obs ####

# Find number of identifiers per class.
tmp <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
tax %>% 
  dplyr::select(id, kingdom, phylum, class) %>% 
  dplyr::rename(taxon.id = id) %>% 
  right_join(., {dplyr::select(obs, id, taxon.id)}) %>% 
  dplyr::filter(!is.na(taxon.id)) %>%
  collect() %>% 
  dbWriteTable(tmp, "obsWithTax", ., overwrite = T)
# Total number identifiers
dplyr::select(ids_no_observer, obs_ID, user.id) %>% 
  dplyr::filter(!is.na(obs_ID)) %>% 
  collect() %>% 
  dbWriteTable(tmp, "ids2", ., overwrite = T)

obsWithTax <- tbl(tmp, "obsWithTax")
ids2 <- tbl(tmp, "ids2")

a <- left_join(ids2, obsWithTax) 
n_IDers <- a %>% 
  dplyr::group_by(kingdom, phylum, class) %>% 
  dplyr::summarise(n_iders = n_distinct(user.id)) %>% 
  collect 

# Ave identifiers per obs
dplyr::select(ids, inat_id, user.id) %>% 
  dplyr::filter(!is.na(inat_id)) %>% 
  dplyr::count(inat_id) %>% 
  collect() %>% 
  dbWriteTable(tmp, "ids3", ., overwrite = T)

ids3 <- tbl(tmp, "ids3")

b <- left_join(ids3, obsWithTax) 
b %>% 
  dplyr::group_by(kingdom, phylum, class) %>% 
  dplyr::summarise(
    mean_IDers = mean(n, na.rm = T),
    median_IDers = median(n, na.rm = T)
  ) %>% 
  collect %>% 
  full_join(n_IDers, .) %>% 
  fwrite(., file = file.path(wd$out, "IDersByTax.csv"))


# Count by taxa --------------

df <- fread(file.path(wd$data, "species_cnts_by_taxon.csv")) %>% 
  filter(!is.na(class_cnt)) %>% 
  na_if("") %>% 
  fill(phylum) %>% 
  fill(kingdom)
df %>% 
  dplyr::select(kingdom, phylum, class, class_cnt) %>% 
  dplyr::filter(class != "Not assigned")

# Disconnect --------------------------------------------------------------

dbDisconnect(iNat)

