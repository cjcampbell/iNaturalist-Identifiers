library(jsonlite)
library(httr)
library(curl)

# Note-- the below code is super messy and full of patches to progressively
# improve things.
# However, it resolved 94.3% of the holes in the taxonomy which originally
# was ~94% complete. So now the taxonomy is expected to be around 99.7% complete
# for all identifications made through 2021. Which I can live with.


file_tax <- "/srv/gspeedq32/mbelitz/iNatIDers/data/tax/inat_taxo.csv"
tax <- lapply(file_tax, fread) %>%
  bind_rows() %>%
  distinct()
tax <- dplyr::rename(tax, taxID = id)

# A lot of rows don't have info.
#toQuery <- tax %>% dplyr::filter(is.na(stateofmatter)) %>% arrange(taxID)
nrow(toQuery)

wd$taxQ <- file.path(wd$bin, "taxQuery")
if(!dir.exists(wd$taxQ)) dir.create(wd$taxQ)

runOrder <- 1:nrow(toQuery)
ticker <- 0

# Function to run through a few times and plug most of the gaps ----------------
# toQuery <- toQuery %>% dplyr::mutate(noAncestry = NA)


# Make new reference combining.
tax2 <- tax %>% 
  rbind(., toQuery, fill = TRUE) %>% 

    dplyr::filter(!is.na(stateofmatter))
#Sys.sleep(9*60*60)

# For each row without taxonomic info, query iNat.
for(i in runOrder) {
  if( is.na( toQuery[i, "stateofmatter"] ) & is.na(toQuery[i,"noAncestry"]) ) {
    if(i%%1000 == 0) {
      fwrite(toQuery, file = file.path(wd$taxQ, paste0(Sys.time(), "Query.csv")))
      #Sys.sleep(60)
    }
    if(ticker %% 1 == 0) { 
      Sys.sleep(1) 
    }
    if(ticker %% 100 == 0) {
      tax2 <- tax %>% 
        rbind(., toQuery, fill = TRUE) %>% 
        dplyr::filter(!is.na(stateofmatter))
    }
    if(ticker >= 10000) {
      fwrite(toQuery, file = file.path(wd$taxQ, paste0(Sys.time(), "Query.csv")))
      stop("Ticker at 10k. Start again tomorrow.")
    }
    ticker <- ticker + 1
    x <- toQuery[i, "taxID"]
    print(paste0("ticker at ", ticker, " while row is ", i, " and ID is ", x, "."))
    try({
      response <- jsonlite::fromJSON(content(GET(paste0("https://www.inaturalist.org/taxa/", x, ".json")), as = "text"))
    })
    if(length(response) == 0) next
    if(is.null(response$ancestry)) {
      toQuery[i,"noAncestry"] <- "1"
      print("Ancestry is not available (taxon swap?). Making a note...")
    }
    if(is.null(response$ancestry)) { next }
    nextUp <- distinct( tax2[ taxID == stringr::word(response$ancestry, -1, sep = "/"), ] )
    # If the next-higher taxa is in the database, use its info to populate the target row.
    if( nrow(nextUp) != 0 ) {
      if(!is.na(nextUp$stateofmatter)) {
        p1 <- data.frame(
          taxID = x,
          taxon_name =  response$name,
          nextUp[ , -c(1,2)]
        )
        p1[ , colnames(p1) == response$rank ] <- response$name
        toQuery[i, ] <- p1
        print("success!")
      }
      if(!is.na(nextUp$noAncestry)) {
        # Note that parent has no Ancestry with 0.5
        toQuery[i, "noAncestry"] <- 0.5
        print("Parent has no Ancestry")
      }
      
    } else if( nrow(nextUp) == 0 ) {
      # If the higher taxa isn't in the database, append it and move that row
      # to the front of the queue
      if(!any(toQuery$taxID == as.numeric(stringr::word(response$ancestry, -1, sep = "/")) )) {
        toQuery <- bind_rows(
          toQuery,
          data.frame(
            taxID = as.numeric(stringr::word(response$ancestry, -1, sep = "/"))
          ),
        )
        runOrder <- c( which(toQuery$taxID == as.numeric(stringr::word(response$ancestry, -1, sep = "/")) ), runOrder)
        print("Adding a new row...") 
      } else {
      # If the higher taxa is in the database, move its row to the front of 
      # the queue.
        runOrder <- c( 
          which(toQuery$taxID == as.numeric(stringr::word(response$ancestry, -1, sep = "/")) ),
          runOrder) %>% 
          unique()
        print("Optimized for next time")
        }
    }
    # If upcoming row is close in number (possibly row i is ancestor),
    # recompile now.
    if(!is.na(toQuery[i+1, "taxID"])) {
      if( abs(toQuery[i, "taxID"]-toQuery[i+1, "taxID"]) < 10 ) {
        tax2 <- tax %>% 
          rbind(., toQuery, fill = TRUE) %>% 
          dplyr::filter(!is.na(stateofmatter)) %>% 
          distinct()
      }
    }
  }
}


#toQuery <- arrange(toQuery, taxID) %>% distinct()

#toQuery <- toQuery %>% dplyr::mutate_if(is.logical, as.character)



# Function to fill in the gaps more conclusively -------------------------------
#i <- 5763
for(i in runOrder) {
  
  if(ticker >= 10000) {
    fwrite(toQuery, file = file.path(wd$taxQ, paste0(Sys.time(), "Query.csv")))
    stop("Ticker at 10k. Start again tomorrow.")
  }

  if( is.na( toQuery[i, "stateofmatter"] ) & is.na(toQuery[i,"noAncestry"]) ) {
    print(paste0("Ticker is at ", ticker, " and row is at ", i, "."))
    x <- toQuery[i, "taxID"]
    if(i %% 1 == 0) {Sys.sleep(1)}
    try({ response <- jsonlite::fromJSON(content(GET(paste0("https://www.inaturalist.org/taxa/", x, ".json")), as = "text")) })
    ticker <- ticker + 1
    # First check if first ancestor data are available.
    if(!is.null(response$ancestry)) {
      ancestor1 <- as.numeric(stringr::word(response$ancestry, -1, sep = "/"))
      # Check if ancestor is in the toQuery sheet or existing taxonomy.
      if(ancestor1 %in% tax2$taxID) {
        nextUp <- distinct( tax2[ taxID == ancestor1, ] )
        p1 <- data.frame(
          taxID = x,
          taxon_name =  response$name,
          nextUp[ , -c(1,2)]
        )
        p1[ , colnames(p1) == response$rank ] <- response$name
        toQuery[i, ] <- p1
        print("direct success!")
      }
      if(ancestor1 %in% toQuery$taxID) {
        # If ancestor is in the toQuery sheet, query it now.
        try({  response1 <- jsonlite::fromJSON(content(GET(paste0("https://www.inaturalist.org/taxa/", ancestor1, ".json")), as = "text")) })
        ticker <- ticker + 1
        # If ancestor has no ancestry of its own, fill in what we know for both.
        if(is.null(response1$ancestry)) {
          toQuery[ toQuery$taxID == ancestor1 , "noAncestry"] <- "1"
          toQuery[ toQuery$taxID == ancestor1 , "taxon_name"] <- response1$name
          toQuery[ toQuery$taxID == ancestor1 , response1$rank] <- response1$name
          
          toQuery[ i , "noAncestry"] <- "0.5"
          toQuery[ i , "taxon_name"] <- response$name
          toQuery[ toQuery$taxID == ancestor1 , response$rank ] <- response$name
          toQuery[ toQuery$taxID == ancestor1 , response1$rank] <- response1$name
          print("Successfully queried 1-level ancestor!")
        } else {
          runOrder <- c(which(toQuery$taxID == ancestor1), i, runOrder)
        }
      }
    }
  }
}


# New option --------------------------------------------------------------

for(i in 1:nrow(toQuery)) {
  
  if(i%%1000 == 0) {
    fwrite(toQuery, file = file.path(wd$taxQ, paste0(Sys.time(), "Query.csv")))
  }
  
  if(ticker >= 10000) {
    fwrite(toQuery, file = file.path(wd$taxQ, paste0(Sys.time(), "Query.csv")))
    stop("Ticker at 10k. Start again tomorrow.")
  }
  
  if( is.na( toQuery[i, "stateofmatter"] ) & is.na(toQuery[i,"noAncestry"]) ) {
    x <- toQuery[i, "taxID"]
    print(paste0("Ticker is at ", ticker, ", row is at ", i, ", and ID is at ", x, "."))
    
    if(x %in% tax2$taxID) {
      stop("here's an example")
    }
    
    if(i %% 1 == 0) {Sys.sleep(1)}
    try({ response <- jsonlite::fromJSON(content(GET(paste0("https://www.inaturalist.org/taxa/", x, ".json")), as = "text")) })
    ticker <- ticker + 1
    
    # First check if first ancestor data are available.
    if(!is.null(response$ancestry)) {
      ancestor1 <- as.numeric(stringr::word(response$ancestry, -1, sep = "/"))
      # Check if ancestor is in the existing taxonomy.
      if(ancestor1 %in% tax2$taxID) {
        nextUp <- distinct( tax2[ taxID == ancestor1, ] )
        p1 <- data.frame(
          taxID = x,
          taxon_name =  response$name,
          nextUp[ , -c(1,2)]
        )
        p1[ , colnames(p1) == response$rank ] <- response$name
        toQuery[i, ] <- p1
        print("direct success!")
      } else {
        # If not, add everything beyond "Life" to the toQuery df.
        anc <- unlist( stringr::str_split(response$ancestry, pattern = "/"))
        anc_tidy <- as.numeric( anc[ !(anc %in% c(48460)) ] )
        toQuery <- bind_rows(
          data.frame(taxID = anc_tidy),
          as.data.frame(toQuery)
        ) %>% 
          distinct()
        print("Added whole ancestry to look up.")
      }
    } else if(is.null(response$ancestry)) {
      toQuery[i,"noAncestry"] <- "1"
      print("Ancestry is not available (taxon swap?). Making a note...")
    }
    tax2 <- tax %>% 
      rbind(., toQuery, fill = TRUE) %>% 
      dplyr::filter(!is.na(stateofmatter)) %>% 
      distinct()
  }
}


# Again -------------------------------------------------------------------

mytax <- tax %>% 
  dplyr::filter(is.na(stateofmatter)) %>%
  arrange(taxID) %>% 
  dplyr::select(taxID) %>% 
  left_join(., toQuery) %>% 
  dplyr::select(-noAncestry) %>% 
  bind_rows(., dplyr::filter(tax, !is.na(stateofmatter))) %>% 
  distinct() %>% 
  group_by(taxID) %>% 
  slice(1)
fwrite(mytax, file.path(wd$out, "taxonomy.csv"))
dbWriteTable(iNat, "tax", mytax, overwrite = T)



# Add indexes -------------------------------------------------------------

dbGetQuery(iNat, "CREATE INDEX taxID on tax(taxID);")    
dbGetQuery(iNat, "PRAGMA index_list(tax);")
