## LIBRARIES 
library(here)
library(dplyr)
library(tidyr) 
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
library(shinydashboard)
library(semantic.dashboard)
library(viridis)
library(RColorBrewer)
library(randomcoloR)
library(leaflet)
library(geojsonio)
library(sf)
library(geojsonsf)
library(httr)
library(jsonlite)


options(scipen=999)

## SETWD
cwd <- here::here()
subdir <- "Documents/dsa/Mod04DataAnalysisR/12-R_ShinyProj/shinyindego"
setwd(here(subdir))
getwd()

## DATA
#2018, 2019, 2020 - Q1, Q2, Q3, Q4 
alldfs <- read.csv(file = '../alldfs2.csv') 
stationsdf <- read.csv(file = 'miscdata/stationsdf.csv') 
top10trips <- read.csv(file = 'miscdata/top10trips_plot.csv')
phlcensus <- read.csv(file = 'miscdata/phlcensus.csv') 
phlbusshelters <- read.csv(file = 'miscdata/shelter_locations.csv') 
phlctpolydata <- geojson_sf('miscdata/phlct3_data.geojson')
phlbikenetwork <- geojson_sf("https://opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson")
phlbslstations <- geojson_sf("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson")
phlmflstations <- geojson_sf("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson")

phlvehcrashes <- geojson_sf("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+crash_data_collision_crash_2007_2017&filename=crash_data_collision_crash_2007_2017&format=geojson&skipfields=cartodb_id")
phlvehcrashes <- select(phlvehcrashes, "dec_lat", "dec_long", "bicycle_death_count", "bicycle_maj_inj_count", "vehicle_count", "person_count", "injury_count", "fatal_count", "crash_year", "crash_month", "day_of_week", "hour_of_day")
phlvehcrashes <- filter(phlvehcrashes, bicycle_death_count > 0 | bicycle_maj_inj_count > 0)
station_heatmap_df <- read.csv(file = 'miscdata/station_heatmap_df.csv')


## FACTORIZE
lapply(alldfs, class)

#stations
alldfs$start_station <- as.factor(alldfs$start_station)
alldfs$end_station <- as.factor(alldfs$end_station)

#year
alldfs$start_year <- as.factor(alldfs$start_year)
alldfs$start_year <- 
  factor(
    x = alldfs$start_year, 
    levels = c(2018, 2019, 2020),
    ordered = TRUE)

#month
levels(alldfs$start_month)
alldfs$start_month <- factor(alldfs$start_month, ordered = TRUE, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

#day of week
alldfs$start_dow1 <- ordered(alldfs$start_dow1, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#hour of day
alldfs$start_time_hour <- 
  factor(
    x = alldfs$start_time_hour, 
    levels = c(
      "12AM", "01AM", "02AM", "03AM", "04AM", "05AM",
      "06AM", "07AM", "08AM", "09AM", "10AM", "11AM",
      "12PM", "01PM", "02PM", "03PM", "04PM", "05PM",
      "06PM", "07PM", "08PM", "09PM", "10PM", "11PM"),
    ordered = TRUE)

#station usage
alldfs$start_station_use <- as.factor(alldfs$start_station_use)
alldfs$start_station_use <- factor(alldfs$start_station_use, ordered = TRUE, levels = c("underutilized", "fair utilization", "adequate utilization", "max utilization")) 
stationsdf$start_station_use <- factor(stationsdf$start_station_use, ordered = TRUE, levels = c("underutilized", "fair utilization", "adequate utilization", "max utilization")) 

#plan duration
alldfs$plan_duration <- factor(alldfs$plan_duration, ordered = TRUE, levels = c(1, 30, 180, 365))

#set station map marker color by usage

setMarkerCol <- function(x) {
  sapply(x$start_station_use, function(use) {
    if ( use == "underutilized") {
      'lightgray'
    }
    else if (use == "fair utilization") {
      "lightblue"
    }
    else if (use == "adequate utilization") {
      "blue"
    }
    else {
      "darkblue"
    }
  })
}

#

## COLORS/ PALETTES
hrno <- 24
hrcolor <- colorRampPalette(brewer.pal(8, "Set2"))(hrno)

n <- 153
randompalette <- distinctColorPalette(n)

pal <- colorNumeric("viridis", NULL)
pal2 <- colorNumeric("BuPu", NULL)

## VARIABLES

chartVars <- c("trip_route_category", "passholder_type", "start_year", "quarter", "start_time_hour", "start_month", "start_dow1", "start_dow_cat", "duration_cat2", "start_station_use", "bike_type")

censusVars <- c("pop density", "pop (%black)", "pop (%nonwhite)", "pop (%white)", "median hh income", "workers16+ (%)", "commute-bike(%)", "commute-walk(%)", "commute-publictransit(%)", "commute-car(%)")

#population density = (B01003_001.POP/ALAND10)*100
#population (% black) = (B02001_003.RACE_B/B01003_001.POP)*100
#population (% nonwhite) = ((B01003_001.POP-B02001_002.RACE_W)/B01003_001.POP)*100
#population (% white) = (B02001_002.RACE_W/B01003_001.POP)*100
#median hh income = B19013_001.INCOME
#workers (16+) = (B08016_001.WORKERS16/B01003_001.POP)*100
#commute (bike) = (B08134_111.CM_BIKE/B08016_001.WORKERS16)*100
#commute (walk) = (B08134_101.CM_WALK/B08016_001.WORKERS16)*100
#commute (car) = (B08134_011.CM_CAR/B08016_001.WORKERS16)*100
#commute (public transit) = (B08134_061.CM_PUB/B08016_001.WORKERS16)*100

## for trip heatmap
#create table for start/end station
station_heatmap_tab <- table(alldfs$start_station_name, alldfs$end_station_name)
#create dataframe
station_heatmap_df <- data.frame(station_heatmap_tab[1:154, 1:153])
#rename columns
names(station_heatmap_df) <- c("start_station", "end_station", "volume")

#filter out the outlier (PMA-PMA route) that screws up entire heatmap #117891 trips - ready to plot
station_heatmap_df <- filter(station_heatmap_df, volume != "117891")
#write.csv(station_heatmap_df, 'miscdata/station_heatmap_df.csv')













