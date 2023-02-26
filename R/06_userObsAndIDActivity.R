
source('/srv/gspeedq32/mbelitz/iNatIDers/R/00_setup.R')

print(paste0( "Beginning process query for script 6 at ", Sys.time()) )
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")

dbSendQuery(
  iNat,
  paste0(
    "CREATE TABLE idsByUser AS
      SELECT
        `id_user.id` AS `user.id`,
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
              COUNT(DISTINCT `obs_ID`) AS `n_obs_IDed`,
              COUNT(*) AS `n_IDs`,
              COUNT(DISTINCT(`ids_taxonID`)) AS `n_taxa_IDed`
            FROM
              (SELECT DISTINCT * FROM ids)
            WHERE `is_observer` = 0
            GROUP BY `id_user.id`
            )
          ) AS LHS
        INNER JOIN
          (
          SELECT
            COUNT(*) AS `n_obs`,
            COUNT(DISTINCT(`obs_taxonID`)) AS `n_taxa_observed`,
            `obs_user.id`
          FROM
            (SELECT DISTINCT * FROM obs)
          GROUP BY `obs_user.id`
          ) AS RHS
        ON LHS.`id_user.id` = RHS.`obs_user.id`
    "
  )
)
df <- dbGetQuery(iNat, "SELECT * FROM idsByUser")
data.table::fwrite(df, file = file.path(wd$out, "idsByUser.csv"))

print(paste0( "Concluding process at ", Sys.time()) )
dbDisconnect(iNat)