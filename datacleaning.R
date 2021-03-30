# CLEAR ENVIRONMENT
rm(list = ls())

# LOAD LIBRARIES
library(here)
library(dplyr)
library(ggplot2)

# DEFINE DIRECTORY 
# https://github.com/jennybc/here_here
cwd <- here::here()

# DEFINE SUBDIRECTORIES
subdir <- "Documents/dsa/Mod04DataAnalysisR/indego"
data_subdir <- "data"

# SET DIRECTORY
setwd(here(subdir))
getwd()

csv_filenames <- list.files('./data', pattern = 'csv')
csv_filepaths <- paste0( './data/', csv_filenames, sep = '' )

all_dfs <- lapply( csv_filepaths, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )

