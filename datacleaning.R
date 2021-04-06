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
subdir <- "Documents/dsa/Mod04DataAnalysisR/12-R_ShinyProj/shinyindego"
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

#head(list_dfs)
#colnames(list_dfs[[5]])
#ncol(list_dfs[[5]]) #15
#nrow (list_dfs[[5]])
#head (list_dfs[[5]]) #2019 Q1
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
#$bike_type [1] "character" -- add this column to 2018 Q1 and Q2 data
#$start_date [1] "Date"

# EXPLORE DATA

# 1. bike_type column missing in 2018 Q1 and Q2 dataset, add column and set all as standard 
# 2. unique values - passholder_type, plan_duration, trip_route_category - drop null
# trip duration categories
# start time - day/night, hours?
# 3. start_time - includes date and time - separate out date and time, add day
# 4. separate out weekday/ weekend


# GET UNIQUE VALUES in the following columns: passholder_type plan_duration trip_route_category
library(dplyr)
library(purrr) #https://www.rdocumentation.org/packages/purrr/versions/0.2.5/topics/flatten

map(list_dfs,  pluck, "passholder_type") %>%
  flatten_chr %>%s
  unique

#[1] "Indego30"     "Indego365"    "Walk-up"     
#[4] "IndegoFlex"   "One Day Pass" "Day Pass"    
#[7] "NULL"   

sum(list_dfs[[6]]$passholder_type == "NULL") #35
unique(list_dfs[[11]]$passholder_type)
#2018 Q1 "Indego30"   "Indego365"  "Walk-up"  "IndegoFlex" "One Day Pass"
#2018 Q2 "Indego30"   "Indego365"  "Walk-up"  "IndegoFlex" "Day Pass"  
#2019 Q2 "Indego30"   "Indego365"  "Day Pass" "IndegoFlex" "NULL" "Walk-up"  
#2019 Q3 "Indego30"   "Indego365"  "Day Pass" "IndegoFlex"
#2020 Q3 "Indego30"   "Indego365"  "Day Pass" 

map(list_dfs,  pluck, "plan_duration") %>%
  flatten_chr %>%
  unique

#[1] "30"   "365"  "0"    "1"    "2"    "NULL"
#[7] "180" 

unique(list_dfs[[10]]$plan_duration)
#2018 Q1 - 30 365   0   1
#2019 Q2 - "30"   "365"  "1"    "2"    "NULL"
#2019 Q3 - 30 365   1   180
#2020 Q2 - 30   1 365
#2020 Q4 - 365  30   1

map(list_dfs,  pluck, "trip_route_category") %>%
  flatten_chr %>%
  unique
#"One Way"    "Round Trip"



list_dfs[1] #2018-06-30 23:58:00
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

#add day of the week
#wday component of a POSIXlt object is the numeric weekday, label=TRUE for days
#wday("2018-03-31", label = TRUE)
add_dowcol <- function(df)
  df %>% mutate (
    start_dow = wday(start_date, label = TRUE)
  )

list_dfs <- lapply(list_dfs, add_dowcol)

head(list_dfs[12])

#check
list_dfs[6] 
list_dfs[10]

#create year and quarter column
add_yearcol <- function(df)
  df %>% mutate(
    start_year = format(start_time, format="%Y"))

list_dfs <- lapply(list_dfs, add_yearcol)

add_quartercol <- function(df)
  df %>% mutate(
    quarter = case_when(
         month(start_date) >= 01 & month(start_date) <= 03 ~ "Q1",
         month(start_date) >= 04 & month(start_date) <= 06 ~ "Q2",
         month(start_date) >= 07 & month(start_date) <= 09 ~ "Q3",
         month(start_date) >= 10 & month(start_date) <= 12 ~ "Q4",
         TRUE ~ NA_character_)
)

list_dfs <- lapply(list_dfs, add_quartercol)

# Clean time
# strptime {base} convert between character representations and objects of classes "POSIXlt" and "POSIXct" representing calendar dates and times.
strftime(x = "7/1/2019 0:01", format = "%I%p")

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
#remove duplicated elements in list of dataframes
stations_df <- lapply(stations_df, function(x) x[!duplicated(x)])

#combine list of dataframes to one dataframe 
stations_df_tot <- rbindlist(stations_df)
head(stations_df_tot)
nrow(stations_df_tot)
lapply(stations_df_tot, class)

#use only start_stations
stations_df_tot <- stations_df_tot %>% select(start_station, start_lat, start_lon)

#only get unique stations - want coordinates of the unique stations in the table
stations_df_tot <- distinct(stations_df_tot, .keep_all = FALSE)
nrow(stations_df_tot) #169

#make sure columns are numeric
stationtable$Station_ID <- as.numeric(stationtable$Station_ID)
stations_df_tot$start_station <- as.numeric(stations_df_tot$start_station)

#join indego station table to stations_df_tot 
idgstations <- full_join(stationtable, stations_df_tot, by = c("Station_ID" = "start_station"))

#write.csv(idgstations,'miscdata/idgstations.csv')

#GET CENSUS TRACT OF STATIONS
#https://stackoverflow.com/questions/51499410/retrieve-census-tract-from-coordinates

idgstations2 <- read.csv(file = 'miscdata/idgstations.csv')


### GET CENSUS DATA ###

#install.packages("tigris")
library(tigris)

#get census tract/ FIPS code for each of the stations using tigris

#use replacement_function in place of the tigris call_geolocator_latlon() function
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

head(idgstations)
colnames(idgstations)

#trim down dataset to just Station_ID and coordinates
idgcoords <- idgstations %>% select(Station_ID, start_lat, start_lon)

#eliminate na in table for replacement_function to work
idgcoords <- na.omit(idgcoords)
head(idgcoords)
nrow(coords) #165


#create new column FIPScode - (patience required)
idgcoords$FIPScode <- apply(idgcoords, 1, function(row) replacement_function(row['start_lat'], row['start_lon']))

#States and the territories are identified by a 2-digit code.
# • Counties within states are identified by a 3-digit code.
# • Tracts within counties are identified 6-digit code.
# • Blocks within tracts are identified by a 4-digit code.

#42 PENNSYLVANIA
#101 Philadelphia County

#extract out census tract from FIPScode
idgcoords$state <- substr(idgcoords$FIPScode, 1,2)
idgcoords$county <- substr(idgcoords$FIPScode, 3,5)
idgcoords$censustract <- substr(idgcoords$FIPScode, 6, 11)
idgcoords$censusblock <- substr(idgcoords$FIPScode, 12,15)
idgcoords$fips <- substr(idgcoords$FIPScode, 1,15)
head(idgcoords)

#remove original FIPScode column - class = list - prevents write.csv
idgcoords <- within(idgcoords, rm(FIPScode))

#fips codes all have class of "character"
lapply(idgcoords, class)

#remove()

#idgcoords$censustract <- as.numeric(idgcoords$censustract) 
#this will remove leading zeroes
#numeric drops zeroes - how to add leading zeros - keep as character for now

#write.csv(idgcoords,'miscdata/idgstations_fips.csv')

### CENSUS API
library(tidycensus)
library(tidyverse)
library(viridis)

census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

#see link below for table/ variables code
#https://api.census.gov/data/2019/acs/acs5/variables.html

phl_income <- get_acs(geography = "tract", variables = "B19013_001", state = "PA", county = "Philadelphia")#, geometry = TRUE)

phl_pop <- get_acs(geography = "tract", variables = "B01003_001", state = "PA", county = "Philadelphia")

phl_race_white <- get_acs(geography = "tract", variables = "B02001_002", state = "PA", county = "Philadelphia")

phl_race_black <- get_acs(geography = "tract", variables = "B02001_003", state = "PA", county = "Philadelphia")

#MEANS OF TRANSPORTATION TO WORK - Taxicab, motorcycle, bicycle, or other means
phl_commute_bike <- get_acs(geography = "tract", variables = "B08134_111", state = "PA", county = "Philadelphia")

#MEANS OF TRANSPORTATION TO WORK - Public transportation (excluding taxicab):
phl_commute_public <- get_acs(geography = "tract", variables = "B08134_061", state = "PA", county = "Philadelphia")

#MEANS OF TRANSPORTATION TO WORK - Car, truck, or van
phl_commute_car <- get_acs(geography = "tract", variables = "B08134_011", state = "PA", county = "Philadelphia")

#MEANS OF TRANSPORTATION TO WORK - walked
phl_commute_walked <- get_acs(geography = "tract", variables = "B08134_101", state = "PA", county = "Philadelphia")

#Age proxy
phl_workers16 <- get_acs(geography = "tract", variables = "B08016_001", state = "PA", county = "Philadelphia")

head(phl_workers16$GEOID) #"42101000100" "42101000200" "42101000300" 
head(phl_pop)
head(phl_commute_bike)
head(phl_commute_walked)
head(phl_commute_car)
head(phl_commute_public)
head(phl_race_white)
head(phl_race_black)
head(phl_income$variable)
sum(phl_pop$estimate) #1,579,075

#join tables/ change column names
require(data.table)
?setnames
setnames(phl_pop, old = "estimate", new = "B01003_001 POP")
setnames(phl_income, old = "estimate", new = "B19013_001 INCOME")
setnames(phl_race_black, old = "estimate", new = "B02001_003 RACE_B")
setnames(phl_race_white, old = "estimate", new = "B02001_002 RACE_W")
setnames(phl_workers16, old = "estimate", new = "B08016_001 WORKERS16")
setnames(phl_commute_bike, old = "estimate", new = "B08134_111 CM_BIKE")
setnames(phl_commute_walked, old = "estimate", new = "B08134_101 CM_WALK")
setnames(phl_commute_car, old = "estimate", new = "B08134_011 CM_CAR")
setnames(phl_commute_public, old = "estimate", new = "B08134_061 CM_PUB")

phlcensuslst <- list(phl_pop, phl_income, phl_race_white, phl_race_black, phl_commute_bike, phl_commute_car, phl_commute_public, phl_commute_walked, phl_workers16)

#phlcensuscol <- c("B01003_001 POP", "B19013_001 INCOME", "B02001_003 RACE_B", "B02001_002 RACE_W", "B08016_001 WORKERS16", "B08134_111 CM_BIKE", "B08134_101 CM_WALK", "B08134_011 CM_CAR","B08134_061 CM_PUB")

phlcensuslst <- lapply(phlcensuslst, function(x) x%>% select(-c("NAME", "variable", "moe")))

phlcensus_dftot <- phlcensuslst %>%
  Reduce(function (df1, df2) left_join(df1, df2, by="GEOID"), .)

colnames(phlcensus_dftot)
head(phlcensus_dftot)

#create census tract column
phlcensus_dftot <- 
  phlcensus_dftot %>% 
  mutate(
    ctract = substr(GEOID, 6,12)
  )

head(phlcensus_dftot$ctract)

#write.csv(phlcensus_dftot,'miscdata/phlcensus_dftot.csv')

### PHILADELPHIA NEIGHBORHOODS - find out what neighborhoods stations are located within
#https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
#https://cran.r-project.org/web/packages/geojsonR/vignettes/the_geojsonR_package.html

library(magrittr)
library(ggplot2)
library(sf)

#phl_nbhds <- read_sf("./miscdata/Neighborhoods_Philadelphia.geojson")
#head(phl_nbhds$geometry)
#colnames(phl_nbhds)

library(geojsonsf)
phl_nbhds <- geojsonsf::geojson_sf("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson")

head(phl_nbhds) 
dimnames(idgcoords)
dimnames(idglatlon)
nrow(phl_nbhds) #158

phl_nbhds %>%
  ggplot() +
  geom_sf(aes(fill = listname))

#extract out - only lat/lon
idglatlon <- idgcoords %>% select(start_lat, start_lon)

# PLOT MAP OF PHL NEIGHBORHOODS
#read the geoJson file that is stored on the web with the geojsonio library:
#https://www.r-graph-gallery.com/325-background-map-from-geojson-format-in-r.html
#install.packages("geojsonio")
#library(geojsonio)
#create a geospatial object called spdf
#spdf <- geojson_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson",  what = "sp")

#'fortify' the data to get a dataframe format required by ggplot2
#geospatial object thus needs to be transformed using the tidy() function of the broom package.
#library(broom)
#spdf_fortified <- tidy(spdf) #Regions defined for each Polygons

#head(spdf)
#head(spdf_fortified)
#colnames(spdf_fortified)

#install.packages("mapproj")
#ggplot() +
#  geom_polygon(data = spdf_fortified, 
#               aes( x = long, y = lat, group = group), 
#               fill="#69b3a2", 
#               color="white") +
#  theme_void() +
#  coord_map()







