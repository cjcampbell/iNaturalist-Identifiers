
source('/srv/gspeedq32/mbelitz/iNatIDers/R/00_setup.R')
library(purrr)
print(paste0( "Beginning process query for script 5 at ", Sys.time()) )
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")

# Obs by date -------------------------------------------------------------

print("Counting observations by date")

obsByDate <- dbGetQuery(
  iNat, 
  paste0(
    "
  SELECT 
    `n_obs`, `yr_precise`, SUM(`n_obs`) 
  OVER (
    ORDER BY `yr_precise` ROWS UNBOUNDED PRECEDING
    ) AS `cum_obs`
  FROM
  (
    SELECT
        CAST(`obs_created_at_year` AS NUMERIC) + CAST(`obs_created_at_month` AS FLOAT) / 12 AS `yr_precise`,
        COUNT(*) AS `n_obs`
      FROM
        (SELECT DISTINCT * FROM obs)
      GROUP BY `obs_created_at_year`, `obs_created_at_month`
  )
  "
  )
)
saveRDS(obsByDate, file = file.path(wd$bin, "obsByDate.rds"))



# IDs made for others by date ---------------------------------------------

print("Counting identifications made for others by date")

idsByDate <- dbGetQuery(
  iNat, 
  paste0(
    "
  SELECT 
    `n_ids`, `yr_precise`, SUM(`n_ids`) 
  OVER (
    ORDER BY `yr_precise` ROWS UNBOUNDED PRECEDING
    ) AS `cum_IDs`
  FROM
  (
    SELECT
        CAST(`id_created_at_year` AS NUMERIC) + CAST(`id_created_at_month` AS FLOAT) / 12 AS `yr_precise`,
        COUNT(*) AS `n_ids`
      FROM
        (SELECT DISTINCT * FROM ids)
      WHERE `is_observer` = 0
      GROUP BY `id_created_at_year`, `id_created_at_month`
  )
  "
  )
)
saveRDS(idsByDate, file = file.path(wd$bin, "idsByDate.rds"))


# n Identifiers IDing for others by date ----------------------------------

IDersByDate <- dbGetQuery(
  iNat, 
  paste0(
    "
  SELECT 
    `n_IDers`, `yr_precise`, SUM(`n_IDers`) 
  OVER (
    ORDER BY `yr_precise` ROWS UNBOUNDED PRECEDING
    ) AS `cum_IDers`
  FROM
  (
    SELECT
        CAST(`id_created_at_year` AS NUMERIC) + CAST(`id_created_at_month` AS FLOAT) / 12 AS `yr_precise`,
        COUNT(DISTINCT(`id_user.id`)) AS `n_IDers`
      FROM
        (SELECT DISTINCT * FROM ids)
      WHERE `is_observer` = 1
      GROUP BY `id_created_at_year`, `id_created_at_month`
  )
  "
  )
)

saveRDS(IDersByDate, file = file.path(wd$bin, "IDersByDate.rds"))


# Proportion with vision - 1st ID -----------------------------------------
prop_vision <- dbGetQuery( iNat, 
            "SELECT `yr_precise`, COUNT(*) AS `n_ids`, is_observer, SUM(vision) AS `n_vision` FROM
              (SELECT vision, is_observer, CAST(`id_created_at_year` AS NUMERIC) + CAST(`id_created_at_month` AS FLOAT) / 12 AS `yr_precise` FROM ids)
              GROUP BY `yr_precise`, `is_observer`;")
prop_vision %>% 
  dplyr::mutate(n_novision = n_ids-n_vision) %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(cols = c(n_vision, n_novision)) %>% 
  dplyr::select(yr_precise, is_observer, name, value) %>% 
  saveRDS(., file = file.path(wd$bin, "prop_vision.rds"))


# Combine and conclude ----------------------------------------------------------------

dbDisconnect(iNat)

# Combine outputs.
df_l <- lapply(c("obsByDate.rds", "idsByDate.rds", "IDersByDate.rds"), function(x) {
  readRDS(file.path(wd$bin, x))
  }) 

df <- purrr::reduce(df_l, dplyr::full_join, by = "yr_precise")
write.csv(df, file = file.path(wd$out, "cumulativeEventsByDate.csv"), row.names = F)


print(paste0( "Concluding process at ", Sys.time()) )
