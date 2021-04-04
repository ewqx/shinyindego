## TITLE HERE

# CLEAR ENVIRONMENT
rm(list = ls())

# LOAD LIBRARIES
library(here)
library(dplyr)
library(tidyr) 
library(ggplot2)
library(lubridate)
library(data.table)  

# DEFINE DIRECTORY 
# https://github.com/jennybc/here_here
cwd <- here::here()
cwd

# DEFINE SUBDIRECTORIES
subdir <- "/Users/estheroids/Documents/dsa/Mod04DataAnalysisR/12-R_ShinyProj/shinyindego"
#data_subdir <- "data"

# SET DIRECTORY
setwd(here(subdir))
getwd()

# LOAD DATA
### Indego Data ###
# Data source: https://www.rideindego.com/about/data/
'Indego, Philadelphia’s bike-share program, offers 24-hour, 30-day, and yearly pass options to ride one of the hundreds of bicycles stationed at more than 140 stations around the city.' 

#2018, 2019, 2020
#Q1 (January – March)
#Q2 (April – June)
#Q3 (July – September)s
#Q4 (October – December)

#make a character vector of all csv file names in the ./data directory
csv_fnames <- list.files('./data', pattern = 'csv')

#create character vector appending ./data/ to each entry of csv_fnames so each one would be the full file path
csv_fpaths <- paste0('./data/', csv_fnames, sep = '')

#read each of the file paths in the vector into a dataframe object, storing all the data frames into a single list
list_dfs <- lapply(csv_fpaths, fread)
#all_dfs <- lapply( csv_fpaths, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )

head(list_dfs)
colnames(list_dfs[[5]])
ncol(list_dfs[[5]]) #15
nrow (list_dfs[[5]])
head (list_dfs[[5]]) #2019 Q1
lapply(list_dfs[[5]], class)
lapply(list_dfs[[7]], class)

#$trip_id [1] "integer"
#$duration [1] "integer"
#$start_time [1] "POSIXct" "POSIXt" [1] "character"
#$end_time [1] "POSIXct" "POSIXt" [1] "character"
#$start_station [1] "integer"
#$start_lat [1] "numeric"
#$start_lon [1] "numeric"
#$end_station [1] "integer"
#$end_lat [1] "numeric"
#$end_lon [1] "numeric"
#$bike_id [1] "integer"
#$plan_duration [1] "integer"
#$trip_route_category [1] "character"
#$passholder_type [1] "character"
#$bike_type [1] "character"
#$start_date [1] "Date"

list_dfs[6] #2019-04-01 00:01:00 
list_dfs[7] #7/1/2019 0:01

#create date column - extract year from start_time
clean_dt <- function(dt){
  require(lubridate)
  parse_date_time (dt, orders = c('YmdHMS','mdYHM') )
}

fmt_df_dt <- function(x) 
  x %>% mutate_at(vars(start_time, end_time), clean_dt)

list_dfs <- lapply(list_dfs, fmt_df_dt)

add_datecol <- function(df)
  df %>% mutate(
    start_date = as.Date(start_time))

list_dfs <- lapply(list_dfs, add_datecol)

head(list_dfs)
list_dfs[6] 
list_dfs[10]

#create year and quarter column

add_yearcol <- function(df)
  df %>% mutate(
    start_year = format(start_time, format="%Y"))

list_dfs <- lapply(list_dfs, add_yearcol)

#LOAD INDEGO STATION INFO CSV
stationtable <- read.csv(file = 'miscdata/indego-stations-2021-01-01.csv')
head(stationtable)
nrow(stationtable) #157 stations
lapply(stationtable, class)
#$Station_ID [1] "integer"
#$Station_Name [1] "factor"
#$Go_live_date [1] "factor"
#$Status [1] "factor"

stations_df <- lapply(list_dfs, function(x) x%>% select(start_station, start_lat, start_lon, end_station, end_lat, end_lon)) 

head(stations_df)
#remove duplicated elements
stations_df <- lapply(stations_df, function(x) x[!duplicated(x)])

#combine list of dataframes to one dataframe 
stations_df_tot <- rbindlist(stations_df)
head(stations_df_tot)
nrow(stations_df_tot)
lapply(stations_df_tot, class)

stations_df_tot <- stations_df_tot %>% select(start_station, start_lat, start_lon)
stations_df_tot <- distinct(stations_df_tot, .keep_all = FALSE)
nrow(stations_df_tot) #169

#join indego station table to stations_df_tot
# make sure columns are numeric
stationtable$Station_ID <- as.numeric(stationtable$Station_ID)
stations_df_tot$start_station <- as.numeric(stations_df_tot$start_station)

idgstations <- full_join(stationtable, stations_df_tot, by = c("Station_ID" = "start_station"))

#write.csv(idgstations,'miscdata/idgstations.csv')

#GET CENSUS TRACT OF STATIONS
#https://stackoverflow.com/questions/51499410/retrieve-census-tract-from-coordinates

idgstations2 <- read.csv(file = 'miscdata/idgstations.csv')

#install.packages("tigris")
library(tigris)

#https://stackoverflow.com/questions/65795510/r-call-geolocator-latlon-function-returns-na
replacement_function <- function (lat, lon, benchmark, vintage) 
{
  if (missing(benchmark)) {
    benchmark <- "Public_AR_Census2020"
  }
  else {
    benchmark <- benchmark
  }
  if (missing(vintage)) {
    vintage <- "Census2020_Census2020"
  }
  else {
    vintage <- vintage
  }
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"
  url <- paste0("x=", lon, "&y=", lat)
  benchmark0 <- paste0("&benchmark=", benchmark)
  vintage0 <- paste0("&vintage=", vintage, "&format=json")
  url_full <- paste0(call_start, url, benchmark0, vintage0)
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  return(response$result$geographies$`Census Blocks`[[1]]$GEOID)
  if (length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) ==
      0) {
    message(paste0("Lat/lon (", lat, ", ", lon, ") returned no geocodes. An NA was returned."))
    return(NA_character_)
  }
  else {
    if (length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) >
        1) {
      message(paste0("Lat/lon (", lat, ", ", lon, ") returned more than geocode. The first match was returned."))
    }
    return(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID)
  }
}

coords <- idgstations2 %>% select(start_lat, start_lon)
coords <- na.omit(coords)
head(coords)
nrow(coords) #165

#States and the territories are identified by a 2-digit code.
#• Counties within states are identified by a 3-digit code.
#• Tracts within counties are identified 6-digit code.
#• Blocks within tracts are identified by a 4-digit code.

#42 PENNSYLVANIA
#101 Philadelphia County

coords$FIPScode <- apply(coords, 1, function(row) replacement_function(row['start_lat'], row['start_lon']))


coords$state <- substr(coords$FIPScode, 1,2)
coords$county <- substr(coords$FIPScode, 3,5)
coords$censustract <- substr(coords$FIPScode, 6, 11)
coords$censusblock <- substr(coords$FIPScode, 12,15)
coords$fips <- substr(coords$FIPScode, 1,15)
head(coords)

#coords <- within(coords, rm(censustract, censusblock))

lapply(coords, class)

#coords$censustract <- as.numeric(coords$censustract)
#numeric drops zeroes - how to add leading zeros - keep as character for now

#remove FIPScode to write csv. (FIPScode class = list)
#coords <- within(coords, rm(FIPScode)) 
#write.csv(coords,'miscdata/idgstations_fips.csv')

# PHILADELPHIA NEIGHBORHOODS
#https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package

#https://github.com/azavea/geo-data/blob/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson

#
