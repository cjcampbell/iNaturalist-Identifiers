
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")

tax <- tbl(iNat, "tax")
obs <- tbl(iNat, "obs")
ids <- tbl(iNat, "ids")

# # Collect identifications -----------------------------------------------
if(!file.exists(file.path(wd$out, "tax_n_ids_sum.csv"))) {
  tax_n_ids <- ids %>%
    dplyr::select(ids_taxonID) %>%
    left_join(., tax, by = c("ids_taxonID" = "taxID")) %>%
    group_by(kingdom, phylum, class, order, family, genus) %>%
    dplyr::summarise(n_ids = n(), .groups = "keep") %>%
    collect()
  fwrite(tax_n_ids, file = file.path(wd$out, "tax_n_ids.csv"))
  ## Summarize ----
  if(!exists("tax_n_ids")) tax_n_ids <- fread( file.path(wd$out, "tax_n_ids.csv"))
  myranks <- c("kingdom", "phylum", "class", "order", "family", "genus")
  tax_n_ids_sum <- lapply( 1:length(myranks), function(i) {
    x <- myranks[1:i]
    out <- tax_n_ids %>%
      group_by_at(x) %>%
      dplyr::summarise(n_ids = sum(n_ids)) %>%
      dplyr::mutate(
        taxon_rank = myranks[i]
        )
    out$taxon_name <- unlist(out[ , myranks[i]])
    return(out)
  }) %>%
    bind_rows()
  fwrite(tax_n_ids_sum, file = file.path(wd$out, "tax_n_ids_sum.csv"))
}

# Collect observations --------------------------------------------------
if(!file.exists(file.path(wd$out, "tax_n_obs_sum.csv"))) {
  tax_n_obs <- obs %>%
    dplyr::select(obs_taxonID) %>%
    left_join(., tax, by = c("obs_taxonID" = "taxID")) %>%
    group_by(kingdom, phylum, class, order, family, genus) %>%
    dplyr::summarise(n_obs = n(), .groups = "keep") %>%
    collect()
  fwrite(tax_n_obs, file = file.path(wd$out, "tax_n_obs.csv"))
  # Summarize ----
  if(!exists("tax_n_obs")) tax_n_obs <- fread( file.path(wd$out, "tax_n_obs.csv"))
  myranks <- c("kingdom", "phylum", "class", "order", "family", "genus")
  tax_n_obs_sum <- lapply( 1:length(myranks), function(i) {
    x <- myranks[1:i]
    out <- tax_n_obs %>%
      group_by_at(x) %>%
      dplyr::summarise(n_obs = sum(n_obs)) %>%
      dplyr::mutate(
        taxon_rank = myranks[i]
      )
    out$taxon_name <- unlist(out[ , myranks[i]])
    return(out)
  }) %>%
    bind_rows()
  fwrite(tax_n_obs_sum, file = file.path(wd$out, "tax_n_obs_sum.csv"))
}


# Create count info -------------------------------------------------------
if(!exists("tax_n_obs_sum")) {
  tax_n_obs_sum <- fread(file.path(wd$out, "tax_n_obs_sum.csv")) %>%
    dplyr::select(taxon_name, taxon_rank, n_obs, everything())
}
if(!exists("tax_n_ids_sum")) tax_n_ids_sum <- fread( file.path(wd$out, "tax_n_ids_sum.csv"))
if(!exists("taxonomy")) taxonomy <- fread(file.path(wd$out, "taxonomy.csv"))

## unite counts from obs and tax, retaining higher taxonomy columns. ----
### Class ----
classDeets <- tax_n_obs_sum %>%
  dplyr::filter(taxon_rank == "class", !is.na(taxon_name), taxon_name != "") %>%
  dplyr::select(n_obs, kingdom, phylum, class) %>%
  dplyr::rename(n_obs_class = n_obs) %>%
  full_join(., {
    tax_n_ids_sum %>%
      dplyr::filter(taxon_rank == "class", !is.na(taxon_name), taxon_name != "") %>%
      dplyr::select(n_ids, kingdom, phylum, class) %>%
      dplyr::rename(n_ids_class = n_ids)
  }) %>% 
  dplyr::select(n_ids_class, n_obs_class, everything())
dbWriteTable(iNat, "tax_count_class", classDeets)

### Family -----
famDeets <- tax_n_obs_sum %>%
  dplyr::filter(taxon_rank == "family", !is.na(taxon_name), taxon_name != "") %>%
  dplyr::select(n_obs, kingdom, phylum, class, order, family) %>%
  dplyr::rename(n_obs_fam = n_obs) %>%
  full_join(., {
    tax_n_ids_sum %>%
      dplyr::filter(taxon_rank == "family", !is.na(taxon_name), taxon_name != "") %>%
      dplyr::select(n_ids, kingdom, phylum, class, family) %>%
      dplyr::rename(n_ids_fam = n_ids)
  }) %>% 
  dplyr::select(n_ids_fam, n_obs_fam, everything())
dbWriteTable(iNat, "tax_count_fam", famDeets)

### Genus ----
genDeets <- tax_n_obs_sum %>%
  dplyr::filter(taxon_rank == "genus", !is.na(taxon_name), taxon_name != "") %>%
  dplyr::select(n_obs, kingdom, phylum, class, order, family, genus) %>%
  dplyr::rename(n_obs_gen = n_obs) %>%
  full_join(., {
    tax_n_ids_sum %>%
      dplyr::filter(taxon_rank == "genus", !is.na(taxon_name), taxon_name != "") %>%
      dplyr::select(n_ids, kingdom, phylum, class, family, genus) %>%
      dplyr::rename(n_ids_gen = n_ids)
  }) %>% 
  dplyr::select(n_ids_gen, n_obs_gen, everything())
dbWriteTable(iNat, "tax_count_gen", genDeets)

dbDisconnect(iNat)
