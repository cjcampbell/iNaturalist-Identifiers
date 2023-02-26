
# Load libraries used in multiple scripts
library(tidyverse)
library(data.table)
library(DBI)

# Set up navigation helper
# Make an object to help navigate the subdirectories.
my_dir_path <- getwd()
wd <- list()
wd$R       <- file.path( my_dir_path, "R" )
wd$data    <- file.path( my_dir_path, "data" )
wd$obs     <- file.path( my_dir_path, "data", "obs" )
wd$ids     <- file.path( my_dir_path, "data", "IDs" )
wd$tax     <- file.path( my_dir_path, "data", "tax" )
wd$bin     <- file.path( my_dir_path, "bin" )
wd$out     <- file.path( my_dir_path, "out" )
wd$figs    <- file.path( my_dir_path, "figs" )

# Check for presence of subdirectories. Create if needed.
invisible({
  lapply(wd, function(i) if( dir.exists(i) != 1 ) dir.create(i) )
})
