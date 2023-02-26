#### Setup ####
writeLines("Setting up")
library(lubridate)
iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")

df <- dbGetQuery(iNat,  "SELECT * FROM obs WHERE obs_ID = (SELECT MAX(obs_ID) FROM obs);")
studyEnd <- paste(
  paste(df$obs_created_at_year, df$obs_created_at_month,  df$obs_created_at_day, sep = "-"),
  paste(df$obs_created_at_hour, df$obs_created_at_minute, df$obs_created_at_second, sep = ":"),
  sep = " "
  
)
if(!exists("taxDeets")) {taxDeets <- fread(file.path(wd$out, "taxonomy.csv"))}

dropDuplicateCols <- function(df) {
  df[,!duplicated(names(df))]
}

################################## Functions ###################################
writeLines("define functions")
## Get time active from observation creation date to end of study.
makeTimeActive <- function(df) {
  stopifnot(
    c("obs_created_at_year") %in% names(df),
    exists("studyEnd")
    )
    df <- df %>% 
      dplyr::mutate(
        date_obs_created = 
          case_when(!is.na(obs_created_at_year) ~ paste(
            paste(obs_created_at_year, obs_created_at_month,  obs_created_at_day, sep = "-"),
            paste(obs_created_at_hour, obs_created_at_minute, obs_created_at_second, sep = ":"),
            sep = " "
          ),
          TRUE ~ as.character(NA)
          ),
        date_obs_created = case_when(
          !is.na(obs_created_at_year) ~ lubridate::ymd_hms(date_obs_created),
          TRUE ~ as.POSIXct(NA)
          ),
        timeActive = case_when(
          !is.na(obs_created_at_year) ~ as.numeric( ymd_hms(studyEnd) - date_obs_created, units = "days"),
          TRUE ~  as.numeric( NA , units = "days")
        )
      )
  return(df)
}

## Make date_id_created for id tbl -----
makeDateCreated <- function(df) {
  stopifnot(
    "id_created_at_year" %in% names(df)
  )
  df <- df %>% 
    dplyr::mutate(
      date_obs_created = 
        case_when(!is.na(obs_created_at_year) ~ paste(
          paste(obs_created_at_year, obs_created_at_month,  obs_created_at_day, sep = "-"),
          paste(obs_created_at_hour, obs_created_at_minute, obs_created_at_second, sep = ":"),
          sep = " "
        ),
        TRUE ~ as.character(NA)
        ),
      date_obs_created = case_when(
        !is.na(obs_created_at_year) ~ lubridate::ymd_hms(date_obs_created),
        TRUE ~ as.POSIXct(NA)
        ),
      date_id_created = 
        case_when(!is.na(id_created_at_year) ~ paste(
          paste(id_created_at_year, id_created_at_month,  id_created_at_day, sep = "-"),
          paste(id_created_at_hour, id_created_at_minute, id_created_at_second, sep = ":"),
          sep = " "
        ),
        TRUE ~ as.character(NA)
        ),
      date_id_created = case_when(
        !is.na(id_created_at_year) ~ lubridate::ymd_hms(date_id_created),
        TRUE ~ as.POSIXct(NA)
      )
    ) %>% 
    dplyr::select(-c(id_created_at_year, id_created_at_month,  id_created_at_day, id_created_at_hour, id_created_at_minute, id_created_at_second, obs_created_at_year, obs_created_at_month,  obs_created_at_day,
                     obs_created_at_hour, obs_created_at_minute, obs_created_at_second))
  return(df)
}




## Get candidate taxa being proposed (including higher taxa) ---
getCandidateTaxa <- function(myIDs_df) {
  if(!exists("taxDeets")) {taxDeets <- fread(file.path(wd$out, "taxonomy.csv"))}
  taxIDs <- myIDs_df %>% dplyr::select(ids_taxonID) 
  candidates0 <- merge.data.table(
    taxIDs, taxDeets,
    all.x = TRUE, all.y = FALSE,
    by.x = "ids_taxonID",
    by.y = "taxID"
  )
  if(nrow(dplyr::filter(candidates0, stateofmatter == "")) > 0) stop("Taxonomy is incomplete for these candidates")
  candidates1 <- candidates0 %>% 
    dplyr::rename(taxID = ids_taxonID) %>% 
    dplyr::select(-c(taxID, taxon_name)) %>% 
    pivot_longer(cols = everything(), names_to = "rank") %>% 
    distinct %>% 
    dplyr::filter(!{is.na(value)}, value != "") %>% 
    collect %>% 
    as.data.table()
  candidates <- merge.data.table(candidates1, taxonRankLevelInfo, all.x =TRUE, all.y=FALSE, by.x = "rank", by.y = "id_taxon.rank") %>% 
    arrange(desc(id_taxon.rank_level))
  return(candidates)
}


## Infer support level at each relevant rank. ----

taxSupport <- function(myIDs_df, candidates) {
  # Function returns table of support for a given table of identification events.
  # All rows are assumed to be active (not withdrawn) at the time of calculation.
  # candidates should be a three-column dataframe containing columns 'rank',
  # 'value' (candidate taxon.ranks to be independently evaluated), and 'level',
  # the associated taxonrank level of the given rank.
  
  obs_IDs2 <- merge.data.table(
    as.data.table(myIDs_df), taxDeets,
    all.x = TRUE, all.y = FALSE,
    by.x = "ids_taxonID",
    by.y = "taxID"
  )
  
  lapply(1:nrow(candidates), function(i) {
    
    taxonEvaluated <- unlist(candidates[i,2])
    whichRank      <- unlist(candidates[i,1])
    whichRank_level <- unlist(candidates[i,3])
    whichColThatRank <- as.numeric(which(names(obs_IDs2) == whichRank))
    
    # Identification count, find count of support for same taxon at exact level.
    identificationCount <- sum( obs_IDs2$taxon_id == taxonEvaluated, na.rm = T)
    # Cumulative count, find count of support related to same taxon at any level
    cumulativeCount     <- sum( obs_IDs2[ , ..whichColThatRank] == taxonEvaluated, na.rm = T )
    # Disagreement count, find count of support for a different taxon at  exact level.
    potentialDisagreements <- unlist(obs_IDs2[ , ..whichColThatRank])
    potentialDisagreements <- potentialDisagreements[potentialDisagreements != ""] # Remove blanks.
    disagreementCount   <- sum( potentialDisagreements != taxonEvaluated , na.rm = T)
    # Ancestor disagreements, find count of support for same taxon at higher level where (disagree = T)
    # Any IDs w/ disagreements:
    disagreeing <- merge.data.table(
      obs_IDs2[ which( obs_IDs2$disagreement == 1 ), ],
      taxonRankLevelInfo, 
      all.x = TRUE, all.y = FALSE,
      by.x = 'id_taxon.rank_level', by.y = 'id_taxon.rank_level'
    )
    # Is the ID at a higher taxonomic level than the one being evaluated?
    ancestorDisagreements <- sum(whichRank_level < disagreeing$level)
    
    # Score:
    score <- cumulativeCount / (cumulativeCount + disagreementCount + ancestorDisagreements)
    
    data.frame(candidates[i, ], cumulativeCount, disagreementCount, ancestorDisagreements, score)
  }) %>% 
    bind_rows
}

## Infer quality grade for a given subset of rows. ------

qualityGrade <- function(df, candidates) {
  if(nrow(df) < 2) {
    qualityGrade <- "needs_id"
  } else {
    taxSupportTab <- taxSupport(df, candidates = candidates)
    # Check if any species-level or below taxonomies have support over 2/3 threshold.
    if( any( taxSupportTab[ taxSupportTab$id_taxon.rank_level <= 10, "score"] > 2/3) & any(taxSupportTab[ taxSupportTab$id_taxon.rank_level <= 10, "cumulativeCount"] > 1) ) {
      qualityGrade <- "research"
    } else {
      qualityGrade <- "needs_id"
    }
  }
  return(qualityGrade)
}

## Function to determine taxonomy of current ID. -----
uniteWithTaxDeets <- function(df) {
  # Extracts desired taxonomy info from the data.table output of the function
  #findTimeToQualityGrade.
  
  if(!exists("taxonomy")) {taxonomy <- fread(file.path(wd$out, "taxonomy.csv"))}
  stopifnot(exists("taxonomy"))
  
  d1 <- merge.data.table(df, taxonomy, by = "taxID", all.x = TRUE, all.y = FALSE, )
  out <- dplyr::select(d1, names(df), class, order, family, genus) %>% 
    rename_at(vars(class, order, family, genus),list( ~paste0("tax", tools::toTitleCase(.))))
  return(out)
}



findTimeToQualityGrade <- function(ex = ex, obs_ID = obs_ID) {
  # Function that estimates time to research grade quality grade.
  # @param ex A dataframe containing one row per identification with the following column names: obs_ID, quality_grade, obs_taxonID, id_ID, id_user.login, ids_taxonID, id_user.id, category, id_taxon.rank, disagreement, vision, previous_observation_taxon_id, taxon.is_active, is_observer, date_obs_created, date_id_created, id_taxon.rank_level
  
  # 0. First see if there are any rows (aka any IDs.)
  if(nrow(ex) == 0) {
    
    out <- data.frame(
      obs_ID            = obs_ID,
      id_ID             = NA,
      group             = 0,
      
      estimatedQuality  = "needs_id",
      hitsRG            = 0,
      RGtaxActive       = NA,
      timeToRG          = NA,
      taxRG             = NA
    )
    
  } else {
    stopifnot( ex[1, "obs_ID"] == obs_ID )
  
  if(nrow(ex) > 0 & nrow(ex) < 2) {
    
    # 1. Check if there are multiple IDs. If fewer than 2, the time without agreeing ID
    # is indefinite.
    
    out <- data.frame(
      obs_ID            = obs_ID,
      id_ID             = ex[nrow(ex), "id_ID"],
      group             = 1,
      
      estimatedQuality  = "needs_id",
      hitsRG            = 0,
      RGtaxActive       = NA,
      timeToRG          = NA,
      taxRG             = NA
    )
    
  } else if(
    
    # 2. Next check if first two IDs are agreeing species-level IDs (including
    # by the observer, but not necessarily.) This will be very common.
    
    length(unique(unlist(ex[1:2, "ids_taxonID"]))) == 1 & # Are agreeing and...
    all(unique(ex[1:2, "id_taxon.rank_level"]) <= 10, na.rm = T)       # at or below species-rank level.
  ) {
    referenceRow <- 2
    out <- data.frame(
      obs_ID            = obs_ID,
      id_ID             = ex[referenceRow, "id_ID"],
      group             = 2,
      
      estimatedQuality  = "research",
      hitsRG            = 1,
      RGtaxActive       = ex[referenceRow, "taxon.is_active"],
      timeToRG          = as.numeric(ex[referenceRow, "date_id_created"] - ex[1, "date_obs_created"], units = "days" ),
      taxRG             = ex[referenceRow, "ids_taxonID"]
    )
  } else if(
    
    # 3. If there are not at least two IDs at a id_taxon.rank at or below 
    # 10, estimated quality will be 'needs id'.
    # There are some of exceptions to this, etc...
    
    sum(ex[ , "id_taxon.rank_level"] <= 10, na.rm = T) < 2
    
  ) {
    
    firstLowestID <- ex[min(which(ex[, "id_taxon.rank_level"] == min(ex[, "id_taxon.rank_level"]))), "id_ID"]
    
    out <- data.frame(
      obs_ID            = obs_ID,
      id_ID             = firstLowestID,
      group             = 3,
      
      estimatedQuality  = "needs_id",
      hitsRG            = 0,
      RGtaxActive       = NA,
      timeToRG          = NA,
      taxRG             = NA
    )
  } else {
    # Find the support for each ID until 2/3 support for one below rank_level == 10
    # Case 4. Later IDs hit RG.
    for(i in 1:nrow(ex) ) {
     # print(i)
      try({
        exSubset <- ex[1:i,]
        # Remove all IDs made by the same IDer prior to their most recent ID, for each subset of the data.table.
        nodupes <- exSubset[ !duplicated(exSubset$id_user.id, fromLast = T), ]
        result <- qualityGrade(df = nodupes, candidates = getCandidateTaxa(nodupes) )
      })
      if(exists("result")) {
        if(result == "research") {
          out <-  data.frame(
            obs_ID            = obs_ID,
            id_ID             = ex[i, "id_ID"],
            group             = 4,
            
            estimatedQuality  = "research",
            hitsRG            = 1,
            RGtaxActive       = ex[i, "taxon.is_active"],
            timeToRG          = as.numeric(ex[i, "date_id_created"] - ex[1, "date_obs_created"] , units = "days"),
            taxRG             = ex[i, "ids_taxonID"]
          ); break
        } else if(i == nrow(ex)) {
          # Case 5. Later IDs don't seem to hit RG.
          out <-  data.frame(
            obs_ID            = obs_ID,
            id_ID             = ex[i, "id_ID"],
            group             = 5,
            
            estimatedQuality  = "needs_id",
            hitsRG            = 0,
            RGtaxActive       = NA,
            timeToRG          = NA,
            taxRG             = NA
          )
        }
      }
    }
    # 6. Unknown conditions // flag for further checks.
    if(!exists("out")) {
      out <- data.frame(
        obs_ID            = obs_ID,
        id_ID             = NA,
        group             = 6,
        
        estimatedQuality  = "Unknown",
        hitsRG            = NA,
        RGtaxActive       = NA,
        timeToRG          = NA,
        taxRG             = NA
      )
    }
  }
  }
  
  if(out$group == 0) {
    out <- data.table(
      out, 
      computerVision = 0,
      nIDs           = 0,
      nIDers         = 0
    )
  } else {
    # Retain additional informative columns.
    out <- data.table(
      out, 
      computerVision = ex[1, "vision"],
      nIDs           = length(unique(ex[ , "id_ID"])),
      nIDers         = length(unique(ex[ , "id_user.id"]))
      )
  }
  return(out)
}




################################## /functions ##################################
# Determine whether an observation exists (is active) for a given obsID. -----
max_obsID <- dbGetQuery(iNat,  "SELECT MAX(obs_ID) FROM obs" ) %>% unlist() # Max obsID

# If more rows must be completed, run the following analysis...
if(!all(
  "activeTime" %in% dbListTables(iNat) &
  unlist(dbGetQuery(iNat,  "SELECT COUNT() FROM activeTime" )) == max_obsID
  )
) {
  print("Working on activeTime")
  # Divide into batches.
  x1 <- seq(1,max_obsID, by = 500000)
  x2 <- c(x1[-1] - 1 , max_obsID) 
  
  ptm <- Sys.time()
  for(i in 1:length(x1)) {
    writeLines(paste(i, "of", length(x1)))
    
    # Make table with every possible obs_ID
    dbWriteTable(
      iNat, "activeTime0",
      data.frame(obs_ID =x1[i]:x2[i]) , 
      overwrite = T)
    
    # Find matching existing obs_IDs.
    df_at <- dbGetQuery(
      iNat,
      paste0(
        "SELECT at.`obs_ID` AS `obs_ID`, ob.`obs_ID` AS `obs_ID_ifActive`, `obs_created_at_year`, `obs_created_at_month`, `obs_created_at_day`, `obs_created_at_hour`, `obs_created_at_minute`, `obs_created_at_second` FROM
        activeTime0 AS at
      LEFT JOIN
        (SELECT obs_ID, `obs_created_at_year`, `obs_created_at_month`, `obs_created_at_day`, `obs_created_at_hour`, `obs_created_at_minute`, `obs_created_at_second` FROM obs) AS ob
      ON at.`obs_ID` = ob.`obs_ID`
      "
      )
    )
    df_at <- makeTimeActive(df_at) %>% 
      dplyr::mutate(active = case_when(is.na(obs_ID_ifActive) ~ 0, TRUE ~ 1)) %>%
      tidyr::fill(timeActive, .direction = "up") %>% 
      dplyr::select(active, obs_ID, timeActive)
    
    if(df_at$obs_ID[1] == 1) {
      dbWriteTable(iNat, "activeTime", df_at, overwrite = T)
    } else {
      dbAppendTable(iNat, "activeTime", df_at)
    }
    Sys.sleep(5)
  }
  print(Sys.time() - ptm)
}

dbSendQuery(iNat, "CREATE INDEX activeObs on activeTime(`obs_ID`);")
dbGetQuery(iNat, "PRAGMA index_list(activeTime);")

# Determing quality grade, time active, other metrics for 1M random observations. ----


## 1. Make tbl with 1M random observations ----

howManyObs <- 1000000
if(howManyObs > unlist(dbGetQuery(iNat, "SELECT COUNT() FROM obs_info")) ) {
  if("obs_info" %in% dbListTables(iNat)) { dbRemoveTable(iNat, "obs_info") }
  writeLines("Making table obs_info")
  set.seed(42);dbSendQuery(
    iNat,
    paste0(
      "CREATE TABLE IF NOT EXISTS obs_info AS
        SELECT lhs.obs_ID AS `obs_ID`, `timeActive`, `observed_on`, `obs_taxonID`, `quality_grade`, `num_identification_agreements`, `num_identification_disagreements`, `obs_taxon.rank`, `obs_taxon.rank_level`, `latitude`, `longitude`, `obs_user.id`, `obs_user.login`, `obs_created_at_year`, `obs_created_at_month`, `obs_created_at_day`, `obs_created_at_hour`, `obs_created_at_minute`, `obs_created_at_second` FROM
          (SELECT obs_ID, timeActive FROM activeTime WHERE active = 1 ORDER BY RANDOM() LIMIT ",howManyObs,") AS lhs
        LEFT JOIN
          (SELECT * FROM obs) AS rhs
        ON lhs.obs_ID = rhs.obs_ID;"
    )
  )
  writeLines("Done table obs_info")
}
dbGetQuery(iNat, "CREATE INDEX obs_info_lonlat on obs_info(longitude, latitude);")
dbGetQuery(iNat, "PRAGMA index_list(obs_info);")

## 2. Bring in tax rank level info ----
if(!exists("taxonRankLevelInfo")) {
  taxonRankLevelInfo <- data.frame(
    id_taxon.rank = c("stateofmatter","kingdom","phylum","class","order","subphylum","subclass","family","genus","species","suborder","superfamily","subfamily","tribe","subtribe","infraorder","infraclass","superorder","parvorder","superclass","zoosection","subspecies","subgenus","variety","supertribe","section","complex","subterclass","subsection","hybrid","zoosubsection","epifamily","form","infrahybrid","genushybrid"),
    id_taxon.rank_level = c(80,70,60,50,40,57,47,30,20,10,37,33,27,25,24,35,45,43,34.5,53,34,5,10,5,26,13,11,44,12,10,33.5,32,5,5,20)
  )
}
if(!"taxonRankLevelInfo" %in% dbListTables(iNat)) {
  dbWriteTable(iNat, "taxonRankLevelInfo", taxonRankLevelInfo)
}

## 3. Determine quality grade -------
print("Determine quality grade")
if(!exists("taxDeets")) {taxDeets <- fread(file.path(wd$out, "taxonomy.csv"))}

myrows <- dbGetQuery(iNat,  "SELECT COUNT(*) FROM obs_info" ) %>% unlist()
batchsize <- 10000 ## DO NOT CHANGE THIS WITHOUT CONSIDERING / UPDATING THE LOG (log-QG)
offset <- seq(0, myrows, by = batchsize)
log_con <- file(file.path(wd$bin, "log-QG.txt"), open="a+b")

for(i in 1:length(offset)) {
  
  # Skip chunk if recorded in log.
  logOut <- unlist(stringr::str_split(readLines(file.path(wd$bin, "log-QG.txt")), pattern = "\n"))
  if(i %in% logOut) next
  
  print(paste("Working on batch", i, "of", length(offset)))
  writeLines("Retrieving data from db")
  df <- dbGetQuery(iNat, paste(
    "SELECT RHS.`obs_ID` AS `obs_ID`, `timeActive`, `observed_on`, `obs_taxonID`, `quality_grade`, `num_identification_agreements`, `num_identification_disagreements`, `obs_taxon.rank`, `obs_taxon.rank_level`, `latitude`, `longitude`, `obs_user.id`, `obs_user.login`, `id_ID`, `ids_taxonID`, `id_user.id`, `id_created_at_year`, `id_created_at_month`, `id_created_at_day`, `id_created_at_hour`, `id_created_at_minute`, `id_created_at_second`, `obs_created_at_year`,`obs_created_at_month`, `obs_created_at_day`, `obs_created_at_hour`, `obs_created_at_minute`, `obs_created_at_second`,`category`, RHS.`id_taxon.rank` AS `id_taxon.rank`,`id_taxon.rank_level`, `disagreement`, `vision`, `previous_observation_taxon_id`, `taxon.is_active`, `is_observer`, `id_user.login` FROM 
                    (SELECT * FROM obs_info LIMIT ", batchsize, " OFFSET ", offset[i], ") AS LHS
                  LEFT JOIN ids AS RHS
                    ON LHS.`obs_ID` = RHS.`obs_ID`
                  LEFT JOIN taxonRankLevelInfo AS trli
                    ON RHS.`id_taxon.rank` = trli.`id_taxon.rank`;"
  )
  )
  if(nrow(df) == 0) next
  df <- makeDateCreated(df) %>% 
    dplyr::filter(!is.na(obs_ID))
  whichObs <- unique(df$obs_ID)
  
  writeLines("Finding time to QG")
  result <- bettermc::mclapply(whichObs, mc.cores = 30, FUN = function(x) {
    try({ 
      ex <- df[df$obs_ID == x, ]
      findTimeToQualityGrade(obs_ID = x, ex = ex)
      })
  }) %>% 
    bind_rows()
  
  # # Debugging mode:
  # result <- lapply(whichObs, function(x) {
  #   print(x)
  #   obs_ID = x
  #   ex <- df[df$obs_ID == x, ]
  #   findTimeToQualityGrade(obs_ID = x, ex = ex)
  # }) %>%
  #   bind_rows()
  
  writeLines("Writing to database")
  if(offset[i] == 0) {
    writeLines("Writing new table")
    dbWriteTable(iNat, "QG", result, overwrite = T)
  } else {
    writeLines("Appending to table")
    dbAppendTable(iNat, "QG", result)
  }
  cat(i, file = log_con, sep="\n")
  
  writeLines("Done! :)")
  Sys.sleep(5)
  
}

if(
  (howManyObs-4999) <= unlist(dbGetQuery(iNat, "SELECT COUNT() FROM QG") ) &
  nrow(dbGetQuery(iNat, "PRAGMA index_list(QG);")) < 2
  ) {
  # Add index if close to target number of obs
  writeLines("Adding indexes")
  dbSendQuery(iNat, "CREATE INDEX QG_obs_ID on QG(obs_ID);")  
  dbSendQuery(iNat, "CREATE INDEX QG_id_ID on QG(id_ID);")  
}

## 4. Determine obs1 rank level -----

if(howManyObs > unlist(dbGetQuery(iNat, "SELECT COUNT() FROM obs1rankLevel")) ) {
  writeLines("Making table obs1rankLevel")
  dbRemoveTable(iNat, "obs1rankLevel")
  dbSendQuery(
    iNat,
    paste0(
      "CREATE TABLE IF NOT EXISTS obs1rankLevel AS
      SELECT oi.`obs_ID` AS `obs_ID`, `id_taxon.rank_level` AS `obs1rankLevel` FROM
          (SELECT obs_ID FROM obs_info) AS oi
        LEFT JOIN
           (SELECT * FROM ids GROUP BY obs_ID HAVING MIN(id_ID)) AS ids
        ON oi.`obs_ID` = ids.`obs_ID`
        LEFT JOIN
          (SELECT * FROM taxonRankLevelInfo) AS RHS
        ON ids.`id_taxon.rank` = RHS.`id_taxon.rank`;"
    )
  )
}


# 5. Determine if RG observations leave RG -----------------
# For obs where hitsRG == 1 & estimatedQuality == "research":

# Cases when yes:
# QG == "needs_id", RGtaxRankLevel < taxRankLevel 
#     - not RG at end of study; at a higher tax rank level
# QG == "needs_id", RGtaxRankLevel >= taxRankLevel
#     - not RG at end of study; other reason
# QG == "research", RG_taxID != taxID, RGtaxActive == 1,  RGtaxRankLevel <= taxRankLevel
#     - RG_rankLevel is NOT lower than current taxon rank level

# Cases when no:
# RG_taxID == taxID
#     - taxID has not changed
# RG_taxID != taxID, RGtaxActive == 0
#     - taxRG is not active, assume taxon swap occurred)
# RG_taxID != taxID, RGtaxActive == 1, RGtaxRankLevel > taxRankLevel
#     - taxRG is active, but current rank level is lower. 
#     - Assume that the ID was brought to a lower level w/o disagreement, e.g., species -> subspecies


mdf <- dbGetQuery(iNat,
           "SELECT * FROM (SELECT * FROM QG WHERE hitsRG = 1) AS qg
                LEFT JOIN (SELECT `obs_ID`, `quality_grade`, `obs_taxon.rank_level`, `obs_taxonID` FROM obs) AS obs ON qg.`obs_ID` = obs.`obs_ID`
                LEFT JOIN (SELECT `id_ID`, `id_taxon.rank`, `ids_taxonID` FROM ids) AS ids ON qg.`id_ID` = ids.`id_ID`
                LEFT JOIN (SELECT `id_taxon.rank`, `id_taxon.rank_level` FROM taxonRankLevelInfo) AS trli ON ids.`id_taxon.rank` = trli.`id_taxon.rank` "
)

## Add a quick check if the taxRG is *nested* under the obs_TaxonID. ----
# Searching for cases where the obs_taxonID (taxon ID at the end of the study)
# is nested within the id_ID associated with the obs hitting research grade.
# Essentially, as a quick check for promotion from species to subspecies or some
# finer taxonomic resolution.

df1 <- dbGetQuery(iNat,
           "SELECT * FROM (SELECT `obs_ID` FROM QG WHERE hitsRG = 1) AS qg
              LEFT JOIN (SELECT `obs_ID`, `obs_taxonID` FROM obs) AS obs ON qg.`obs_ID` = obs.`obs_ID`
              LEFT JOIN (SELECT `taxID`,`kingdom`, `phylum`, `order`, `family`, `genus`, `species` FROM tax) AS tax ON obs.`obs_taxonID` = tax.`taxID`"
           ) %>% 
  dropDuplicateCols()
df2 <- dbGetQuery(iNat,
            "SELECT * FROM (SELECT `obs_ID`, `id_ID` FROM QG WHERE hitsRG = 1) AS qg
                LEFT JOIN (SELECT `id_ID`,`ids_taxonID` FROM ids) AS ids ON qg.`id_ID` = ids.`id_ID`
                LEFT JOIN (SELECT `taxID`,`kingdom`, `phylum`, `order`, `family`, `genus`, `species` FROM tax) AS tax ON ids.`ids_taxonID` = tax.`taxID`"
          ) %>% 
  dropDuplicateCols()
fwrite(df2, file = file.path(wd$bin, "nested-df2.csv"))

# `isNested` returns the unque number of kingdom/phylum/.../genus/species
# combinations associated with both obs_taxonID and the RG_id.
# If 1, they are nested or identical. If 0, they are not.
isNested <- bind_rows(df1,df2) %>% 
  dplyr::select(obs_ID, kingdom, phylum, order, family,genus,species) %>% 
  distinct() %>% 
  group_by(obs_ID) %>% 
  dplyr::summarise(isNested=n()) %>% 
  ungroup %>% 
  mutate(isNested = case_when(isNested == 2 ~ 0, isNested == 1 ~ 1))

mdf2 <- mdf[,!duplicated(names(mdf))] %>%         # remove duplicated columns.
  dplyr::rename(                                  # rename for clarity
    RG_id = id_ID,                                # the ID that made the obs hit estimated RG                  
    RG_taxID = taxRG,                             # id_ID is either the most recent ID in estimated needs_ID or the one that makes the obs hit RG in estimated research quality.
    RG_taxon.rank = id_taxon.rank,
    RG_taxon.rank_level = id_taxon.rank_level     # the rank level of the estimated RG ID
    ) %>% 
  dplyr::select(-ids_taxonID) %>% # remove redundant col (same as taxRG)
  left_join(., isNested, by = "obs_ID")

## Unite to determine leavesRG y/n ----

mdf3 <- mdf2 %>% 
  dplyr::mutate(
    leavesRG = case_when(
      # Left RG
      quality_grade == "needs_id" ~ 1,
      quality_grade == "research" & RG_taxID != obs_taxonID & RGtaxActive == 1 & (RG_taxon.rank_level <= obs_taxon.rank_level & isNested == 0) ~ 1,
      # Remains RG
      TRUE ~ 0
    ),
    leavesRG_notes = case_when(
      # Left RG
      quality_grade == "needs_id" & obs_taxon.rank_level > RG_taxon.rank_level ~ "is needs_id; bumped to higher taxon rank level",
      quality_grade == "needs_id" & obs_taxon.rank_level <= RG_taxon.rank_level ~ "is needs_id; other reason",
      quality_grade == "needs_id" & is.na(obs_taxonID) ~ "is needs_id, does not have an active observation taxon",
      quality_grade == "research" & RG_taxID != obs_taxonID & RGtaxActive == 1 & RG_taxon.rank_level <= obs_taxon.rank_level & isNested == 0 ~ "is RG, taxon has changed from an active RG-taxon and is not at a finer taxonomic level",
      # Remains RG
      quality_grade == "research" & RG_taxID != obs_taxonID & RGtaxActive == 1 & (RG_taxon.rank_level > obs_taxon.rank_level | isNested == 1) ~ "is RG, taxon has changed from an active RG-taxon but to a finer taxonomic level",
      quality_grade == "research" & RG_taxID == obs_taxonID ~ "is RG, same obs_taxonID persisted",
      quality_grade == "research" & RG_taxID != obs_taxonID & RGtaxActive == 0 ~ "is RG, likely taxon swap (RG-tax is not active)"
    )
  )
dbWriteTable(iNat, "leavesRG", mdf3, overwrite = T)

# 6. Find identifier experience --------------------------------------------------------

idDeets <- dbGetQuery(
  iNat,
    "SELECT * FROM
        (SELECT `obs_ID` FROM obs_info) AS oi
        LEFT JOIN (SELECT `obs_ID`, `hitsRG`, `id_ID` AS `RG_idID` FROM QG) AS qg ON oi.`obs_ID` = QG.`obs_ID`
        LEFT JOIN (SELECT `obs_ID`, `id_user.id`, `id_ID` FROM ids) AS ids ON oi.`obs_ID` = ids.`obs_ID`
        LEFT JOIN (SELECT `user.id`, `n_IDs` FROM idsByUser) AS exp ON ids.`id_user.id` = exp.`user.id`
        ;
    "
  )

idDeets[,!duplicated(names(idDeets))] %>% 
  na.omit() %>% 
  distinct() %>% 
  group_by(obs_ID) %>% 
  dplyr::summarise(IDexperience_allIDs = sum(n_IDs, na.rm = T)) %>% 
  dbWriteTable(iNat, "IDexperience_allIDs", ., overwrite = T)

# find ID experience made before RG_idID (identification that makes obs hit RG)
idDeets[,!duplicated(names(idDeets))] %>% 
  na.omit() %>% 
  dplyr::filter(hitsRG == 1) %>% 
  group_by(obs_ID) %>% 
  arrange(id_ID) %>% 
  dplyr::filter(id_ID <= RG_idID) %>% 
  dplyr::summarise(IDexperience_preRG = sum(n_IDs, na.rm = T)) %>% 
  dbWriteTable(iNat, "IDexperience_preRG", ., overwrite = T)


# 7. Combine into one large tbl by obs_ID ----------------------------------------------
#source("/home/mbelitz/../../srv/gspeedq32/mbelitz/iNatIDers/R-old/00_setup.R")
#iNat <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNat.db")
# dbRemoveTable(iNat, "obs_info2")
dbSendQuery(
  iNat,
  "CREATE TABLE IF NOT EXISTS obs_info2 AS
      SELECT oi.`obs_ID` AS `obs_ID`, `timeActive`, `observed_on`, tax.`taxID` AS `taxID`, `quality_grade`, `estimatedQuality`, `hitsRG`,`timeToRG`,`RGtaxActive`,`taxRG`,`leavesRG`,`num_identification_agreements`, `num_identification_disagreements`, `obs_taxon.rank`, `obs_taxon.rank_level`, oi.`latitude` AS `latitude`, oi.`longitude` AS `longitude`, `obs_user.id`, `obs_user.login`, `obs1rankLevel`, `n_obs_gen`, `n_ids_gen`, `n_obs_class`, `n_ids_class`,`n_obs_fam`, `n_ids_fam`, `taxon_name`, `taxClass`, `taxOrder`, `taxFamily`,`taxGenus`, `geo_biome`, `nObs_biome`, `geo_ISO3` , `nObs_ISO3` , `geo_gridID`, `nObs_gridID`, `computerVision`, `nIDs`, `nIDers`, `IDexperience_allIDs`, `IDexperience_preRG`, `leavesRG`, `leavesRG_notes` FROM
          (SELECT * FROM obs_info) AS oi
        LEFT JOIN (SELECT * FROM geoObs) AS geo ON oi.`latitude` = geo.`latitude` AND oi.`longitude` = geo.`longitude`
        LEFT JOIN (SELECT * FROM obs1rankLevel) AS obs1rankLevel ON oi.`obs_ID` = obs1rankLevel.`obs_ID`
        LEFT JOIN (SELECT * FROM obsTax_genusCount) AS obsTax_genusCount ON oi.`obs_ID` = obsTax_genusCount.`obs_ID`
        LEFT JOIN (SELECT * FROM obsTax_familyCount) AS obsTax_familyCount ON oi.`obs_ID` = obsTax_familyCount.`obs_ID`
        LEFT JOIN (SELECT * FROM obsTax_classCount) AS obsTax_classCount ON oi.`obs_ID` = obsTax_classCount.`obs_ID`
        LEFT JOIN (SELECT `taxID`, `taxon_name`, `class` as `taxClass`, `order` as `taxOrder`, `family` as `taxFamily`, `genus` as `taxGenus` FROM tax) as tax ON tax.`taxID` = oi.`obs_taxonID`
        LEFT JOIN (SELECT * FROM QG) as qg ON oi.`obs_ID` = qg.`obs_ID`
        LEFT JOIN (SELECT * FROM IDexperience_allIDs) as exp1 ON oi.`obs_ID` = exp1.`obs_ID`
        LEFT JOIN (SELECT * FROM IDexperience_preRG) as exp2 ON oi.`obs_ID` = exp2. `obs_ID`
        LEFT JOIN (SELECT obs_ID, leavesRG, leavesRG_notes FROM leavesRG) AS lrg ON oi.`obs_ID` = lrg.`obs_ID`;"
)

#targetCols <- c("obs_ID","timeActive","obs1rankLevel","n_obs_gen","n_obs_class","n_obs_fam","n_ids_gen","n_ids_class","n_ids_fam","taxID","taxGenus","taxClass","taxFamily","taxOrder","estimatedQuality","hitsRG","timeToRG","RGtaxActive","taxRG","leavesRG","geo_biome","nObs_biome","geo_ISO3","nObs_ISO3","geo_gridID","nObs_gridID","computerVision","nIDs","nIDers","IDexperience_allIDs","IDexperience_preRG")
#