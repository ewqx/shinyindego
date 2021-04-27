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
phlcensus <- read.csv(file = 'miscdata/phlcensus.csv') 
phlctpolydata <- geojson_sf('miscdata/phlct3_data.geojson')
phlbikenetwork <- geojson_sf("https://opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson")


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

#plan duration
alldfs$plan_duration <- factor(alldfs$plan_duration, ordered = TRUE, levels = c(1, 30, 180, 365))

#station map markers by usage

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
    else {"darkblue"}
  })
}

icons <- awesomeIcons(
  icon = 'map-marker-alt',
  iconColor = "#FFFFFF",
  library = 'fa',
  markerColor = setMarkerCol(stationsdf)
)



## COLORS/ PALETTES
hrno <- 24
hrcolor <- colorRampPalette(brewer.pal(8, "Set2"))(hrno)

n <- 153
randompalette <- distinctColorPalette(n)

pal <- colorNumeric("viridis", NULL)
pal2 <- colorNumeric("BuPu", NULL)


## VARIABLES

categoricalVars <- c("trip_route_category", "passholder_type","trip_route_category", "start_year", "quarter", "start_time_hour", "start_month", "start_dow1", "start_dow_cat", "duration_cat2", "start_station_use")

stationVars <- c("trip_route_category", "passholder_type", "start_year", "quarter", "start_time_hour", "start_month", "start_dow1", "start_dow_cat", "duration_cat2", "start_station_use", "bike_type")



