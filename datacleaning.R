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


# GET UNIQUE VALUES in the following columns: passholder_type plan_duration trip_route_category bike_type
library(dplyr)
library(purrr) #https://www.rdocumentation.org/packages/purrr/versions/0.2.5/topics/flatten

map(list_dfs,  pluck, "bike_type") %>%
  flatten_chr %>%
unique
#"standard" "electric"

unique(list_dfs[[2]]$bike_type) 
#2018 Q1 - NULL
#2018 Q2 - NULL
#2018 Q3 - "standard"
#2018 Q4 - "standard" "electric"

#add back bike_type and set to "standard" for 2018 Q1 and Q2

list_dfs[[1]]$bike_type <- rep("standard", times = (nrow(list_dfs[[1]])))
list_dfs[[2]]$bike_type <- rep("standard", times = (nrow(list_dfs[[2]])))

map(list_dfs,  pluck, "passholder_type") %>%
  flatten_chr %>%
  unique

#[1] "Indego30"     "Indego365"    "Walk-up"     
#[4] "IndegoFlex"   "One Day Pass" "Day Pass"    
#[7] "NULL"   

sum(list_dfs[[6]]$passholder_type == "NULL") #35
list_dfs <- list_dfs %>% discard(is.null)
#list_dfs <- list_dfs[!sapply(list_dfs,is.null)]

#remove na values
list_dfs <- lapply(list_dfs, na.omit)
sum(is.na(list_dfs))

unique(list_dfs[[1]]$passholder_type)
#2018 Q1 "Indego30"   "Indego365"  "Walk-up"  "IndegoFlex" "One Day Pass"
#2018 Q2 "Indego30"   "Indego365"  "Walk-up"  "IndegoFlex" "Day Pass"  
#2019 Q2 "Indego30"   "Indego365"  "Day Pass" "IndegoFlex" "NULL" "Walk-up"  
#2019 Q3 "Indego30"   "Indego365"  "Day Pass" "IndegoFlex"
#2020 Q3 "Indego30"   "Indego365"  "Day Pass" 

#rename One Day Pass used in 2018 Q1 and Q2 dataset to Day Pass
rename_colval <- function(df)
  df %>% mutate(passholder_type = ifelse(as.character(passholder_type) == "One Day Pass", "Day Pass", as.character(passholder_type)))

list_dfs <- lapply(list_dfs, rename_colval)
head(list_dfs[[12]])

#look at plan_duration
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


#Check start_time format - different formats
list_dfs[1] #2018-06-30 23:58:00
list_dfs[7] #7/1/2019 0:01

#clean date/time column - extract year from start_time
clean_dt <- function(dt){
  require(lubridate)
  parse_date_time (dt, orders = c('YmdHMS','mdYHM') )
}

fmt_df_dt <- function(x) 
  x %>% mutate_at(vars(start_time, end_time), clean_dt)

list_dfs <- lapply(list_dfs, fmt_df_dt)

head(list_dfs[[1]]$start_time)
head(list_dfs[[12]]$start_time)

fmt_df_dt2 <- function(x) 
  x %>% mutate_at(vars(start_time, end_time), as.POSIXct, format = '%d%b%Y%H%M', tz = "UTC")

parse_dt <- function(dt){
  require(lubridate)
  parse_date_time (dt, orders = "Ymd HMS")
}

fmt_df_dt3 <- function(x) 
  x %>% mutate_at(vars(start_time, end_time), parse_dt)

list_dfs <- lapply(list_dfs, fmt_df_dt2)

lapply(list_dfs[[8]], class)
head(list_dfs[[7]])

#add column with date only
add_datecol <- function(df)
  df %>% mutate(
    start_date = as.Date(start_time))

list_dfs <- lapply(list_dfs, add_datecol)

#add day of the week
#https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
#label=TRUE for days
#returns the days as a factor, so the days will be in the correct order
#wday("2018-03-31", label = TRUE)
add_dowcol <- function(df)
  df %>% mutate (
    start_dow = wday(start_date, label = TRUE)
  )

list_dfs <- lapply(list_dfs, add_dowcol)

unique(list_dfs[[1]]$start_dow) #levels

head(list_dfs[1])
#check
head(list_dfs[6])
head(list_dfs[12])

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

#add hour column - categorize start_time into hours of the day
list_dfs[[12]]
#strftime("7/1/2019 0:01", format = "%H%p") #"12AM"
#strftime("01:52:00 2018-01-01", format = "%H%p") #error
#strftime("2018-01-01 01:52:00", format = "%H%p") #"01AM"
#hour("2018-01-01 01:52:00")
#parse_date_time("2018-01-01 01:52:00", orders= "Ymd HMS")

add_hrcol <- function(df)
  df %>% mutate( 
    sthr = hour(start_time)
  )

list_dfs <- lapply(list_dfs, add_hrcol) 

list_dfs[[6]]

rfmt_hrcol <- function(df)
  df %>% mutate(
    start_time_hour = case_when(
      sthr == 00 ~ "12AM",
      sthr == 1 ~ "01AM",
      sthr == 2 ~ "02AM",
      sthr == 3 ~ "03AM",
      sthr == 4 ~ "04AM",
      sthr == 5 ~ "05AM",
      sthr == 6 ~ "06AM",
      sthr == 07 ~ "07AM",
      sthr == 08 ~ "08AM",
      sthr == 09 ~ "09AM",
      sthr == 10 ~ "10AM",
      sthr == 11 ~ "11AM",
      sthr == 12 ~ "12PM",
      sthr == 13 ~ "01PM",
      sthr == 14 ~ "02PM",
      sthr == 15 ~ "03PM",
      sthr == 16 ~ "04PM",
      sthr == 17 ~ "05PM",
      sthr== 18 ~ "06PM",
      sthr == 19 ~ "07PM",
      sthr == 20 ~ "08PM",
      sthr == 21 ~ "09PM",
      sthr == 22 ~ "10PM",
      sthr == 23 ~ "11PM",
      TRUE ~ NA_character_)
  )

list_dfs <- lapply(list_dfs, rfmt_hrcol)

list_dfs[[7]]

#unique(list_dfs[[10]]$)

alldfs <- rbindlist(list_dfs)

head(alldfs)
colnames(alldfs)
sum(is.na(alldfs))
sum(is.null(alldfs))

alldfs <- alldfs %>% mutate(
  start_dow_cat = case_when(
    start_dow1 == "Sun" | start_dow1 == "Sat" ~ "Weekend",
    TRUE ~ "Weekday"))

max(suballdfs$duration) #1440 min
min(alldfs$duration) #1 min
median(alldfs$duration) #13 min
mean(alldfs$duration) #25.28671 min
summary(alldfs$duration)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    8.00   13.00   25.29   24.00 1440.00 
#suballdfs = filter(alldfs, !(duration %in% c(1440, 1)))

alldfs <- alldfs %>% mutate(
  start_duration_cat = factor((duration %/% 30)+1, ordered=TRUE))
head(alldfs$start_duration_cat)
#[1] 1 1 1 1 1 1
#49 Levels: 1 < 2 < 3 < 4 < 5 < 6 < 7 < 8 < 9 < ... < 49
# 1: trips 30 minutes and under
# 2: trips 60 min and under
summary(alldfs$start_duration_cat)
barplot(table(alldfs$start_duration_cat))

alldfs <- alldfs %>% mutate(
  start_dur_60 = case_when(
    duration <= 30 ~ "Short",
    duration %in% 31:60 ~ "Medium",
    TRUE ~ "Long")) 

#write.csv(alldfs, file = 'miscdata/alldfs.csv')

#### READ alldfs
alldfs <- read.csv(file = '../alldfs.csv') 

#factorize cols
alldfs$start_time_hour <- 
  factor(
    x = alldfs$start_time_hour, 
    levels = c(
      "12AM", "01AM", "02AM", "03AM", "04AM", "05AM",
      "06AM", "07AM", "08AM", "09AM", "10AM", "11AM",
      "12PM", "01PM", "02PM", "03PM", "04PM", "05PM",
      "06PM", "07PM", "08PM", "09PM", "10PM", "11PM"),
    ordered = TRUE)

#delete IndegoFlex and null in passholder_type
alldfs = filter(alldfs,  passholder_type != "IndegoFlex" & passholder_type != "NULL" & passholder_type != "Walk-up")
unique(alldfs$passholder_type)

alldfs = alldfs %>%
  mutate(start_month = month(start_time, label=TRUE))

#alldfs = alldfs %>%
#  mutate(start_dow1 = wday(start_time, label = TRUE, week_start = 1))s


unique(alldfs$start_time_hour) 
unique(alldfs$start_dow1) 
#[1] Tue Thu Sun Mon Wed Sat Fri
# Levels: Fri Mon Sat Sun Thu Tue Wed

alldfs$start_dow1 <- ordered(alldfs$start_dow1, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

colnames(alldfs)

alldfs <- within(alldfs, rm("sthr"))
alldfs <- within(alldfs, rm("start_dow", "X.2", "X", "X.1"))

dimnames(alldfs)
tail(alldfs)

### rewrite alldfs
write.csv(alldfs, file = '../alldfs.csv') 

### PLOT 

#install.packages("wesanderson")
library(wesanderson)
library("viridis")

colnames(alldfs)
#prevent scientific notation
options(scipen=999)

# plot histogram of quarterly bike trips, fill color by year
hist_quarter_year <- ggplot(alldfs, aes(x = quarter)) + 
  stat_count(aes(fill= start_year)) +
  #scale_fill_manual(values = wes_palette("Darjeeling2", n = 3))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Frequency of Bike Trips by Quarter",
    subtitle = "Fill color by year (2018-2020)",
    x = "Quarter",
    y = "Volume",
    fill = "Year"
  )

#plot                  
hist_quarter_year

# plot histogram of yearly bike trips, fill color by pass type
hist_year_pass <- ggplot(alldfs, aes(x = start_year)) + 
  stat_count(aes(fill= passholder_type)) +
  #scale_fill_manual(values = wes_palette("BottleRocket1", n = 6))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Frequency of Bike Trips by Year",
    subtitle = "Fill color by Pass Type",
    x = "Year",
    y = "Volume",
    fill = "Pass Type"
  )

# plot
hist_year_pass

colnames(alldfs)

# plot histogram of bike trips by day of the week, fill color by pass type
hist_dow_pass <- ggplot(alldfs, aes(x = start_dow1)) + 
  stat_count(aes(fill= passholder_type)) +
  #scale_fill_manual(values = wes_palette("BottleRocket1", n = 6))+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Frequency of Bike Trips by  Day of Week",
    subtitle = "Fill color by Pass Type",
    x = "Year",
    y = "Volume",
    fill = "Pass Type"
  )

# plot
hist_dow_pass

# plot histogram of bike trips by hour, fill using dow
#hist_hr_dow <- ggplot(alldfs, aes(x = start_time_hour)) + 
#  stat_count(aes(fill= start_dow1)) +
#  #scale_fill_manual(values = wes_palette("BottleRocket1", n = 6))+
#  scale_color_viridis(discrete = TRUE, option = "D")+
#  scale_fill_viridis(discrete = TRUE) +
#  labs(
#    title = "Frequency of Bike Trips by Hour of the day",
#    subtitle = "Fill color by Day of the week",
#    x = "Hour of the day",
#    y = "Volume",
#    fill = "Day"
#  )

# plot
#hist_hr_dow

library(RColorBrewer)


hist_hr_dow <- ggplot(alldfs, aes(x = start_time_hour)) + 
  stat_count(aes(fill= start_dow1)) +
  #scale_color_viridis(discrete = TRUE, option = "magma")+
  #scale_fill_viridis(discrete = TRUE, option = "magma") +
  scale_fill_brewer(palette = "BuPu", direction=-1) +
  scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Hour of the day",
    subtitle = "Fill color: Day of the week",
    x = "Hour of the day",
    y = "Volume",
    fill = "Day"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot(hist_hr_dow)

#hr of day by pass type
hist_hr_pass <- ggplot(alldfs, aes(x = start_time_hour)) + 
  stat_count(aes(fill= passholder_type)) +
  #scale_color_viridis(discrete = TRUE, option = "magma")+
  #scale_fill_viridis(discrete = TRUE, option = "magma") +
  scale_fill_brewer(palette = "BuPu", direction=-1) +
  scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Hour of the day",
    subtitle = "Fill color: Pass Type",
    x = "Hour of the day",
    y = "Volume",
    fill = "Day"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot(hist_hr_pass)

#hour by quarter - winter months = less usage
hist_hr_q <- ggplot(alldfs, aes(x = start_time_hour)) + 
  stat_count(aes(fill= quarter)) +
  scale_fill_brewer(palette = "BuPu", direction=-1) +
  scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Hour of the day",
    subtitle = "Fill color: Pass Type",
    x = "Hour of the day",
    y = "Volume",
    fill = "Day"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot(hist_hr_q)

#hour by year
hist_hr_yr <- ggplot(alldfs, aes(x = start_time_hour)) + 
  stat_count(aes(fill= start_year)) +
  scale_fill_brewer(palette = "Purples", direction=-1) +
  #scale_color_brewer(palette = "Set2", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Hour of the day",
    subtitle = "Fill color: Pass Type",
    x = "Hour of the day",
    y = "Volume",
    fill = "Day"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot(hist_hr_yr)

class(alldfs$start_year)
alldfs$start_year <- as.factor(alldfs$start_year)
#write.csv(alldfs, file="../alldfs.csv")

length(unique(alldfs$start_station)) #154
#stationno <- 154
#stationcolor <- colorRampPalette(brewer.pal(8, "Set2"))(stationno)
library(randomcoloR)
n <- 154
randompalette <- distinctColorPalette(n)

hist_hr_stat <- ggplot(alldfs, aes(x = start_time_hour)) + 
  stat_count(aes(fill= start_station_name)) +
  scale_fill_manual(values = randompalette) +
  #scale_color_brewer(palette = "Set2", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Hour of the day",
    subtitle = "Fill color: Start Station",
    x = "Hour of the day",
    y = "Volume",
    fill = "Stations"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")  

plot(hist_hr_stat)

alldfs$start_station <- as.factor(alldfs$start_station)
class(alldfs$start_station)

#start station by quarter 
hist_stat_q <- ggplot(alldfs, aes(x = start_station)) + 
  stat_count(aes(fill= quarter)) +
  scale_fill_brewer(palette = "BuPu", direction=-1) +
  scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Station",
    subtitle = "Fill color: quarter",
    x = "Station",
    y = "Volume",
    fill = "quarter"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot(hist_stat_q)

#start station by hour of the day 
class(alldfs$start_time_hour)
levels(alldfs$start_time_hour)

hrno <- 24
hrcolor <- colorRampPalette(brewer.pal(8, "Set2"))(hrno)

hist_sstat_hr <- ggplot(alldfs, aes(x = start_station)) + 
  stat_count(aes(fill= start_time_hour)) +
  scale_fill_manual(values = hrcolor) +
  labs(
    title = "Volume of Bike Trips by Start station",
    subtitle = "Fill color: hour",
    x = "Station",
    y = "Volume",
    fill = "hour"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot(hist_sstat_hr)

# plot station by trip_route category
hist_stat_trc <- ggplot(alldfs, aes(x = start_station)) + 
  stat_count(aes(fill= trip_route_category)) +
  scale_fill_brewer(palette = "Set2") +
 # scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Station",
    subtitle = "Fill color: trip route category",
    x = "Station",
    y = "Volume",
    fill = "trc"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot(hist_stat_trc)

# plot station by pass type
hist_stat_pass <- ggplot(alldfs, aes(x = start_station)) + 
  stat_count(aes(fill= trip_route_category)) +
  scale_fill_brewer(palette = "Set2") +
  # scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Station",
    subtitle = "Fill color: passholder_type",
    x = "Station",
    y = "Volume",
    fill = "pass type"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot(hist_stat_pass)

colnames(alldfs)

# plot station by weekend/weekday
sub_alldfs <- subset(alldfs, start_station!= "3057")

hist_stat_dowcat <- ggplot(alldfs, aes(x = start_station)) + 
  stat_count(aes(fill= start_dow_cat)) +
  scale_fill_brewer(palette = "Set2") +
  # scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Station",
    subtitle = "Fill color: day category",
    x = "Station",
    y = "Volume",
    fill = "day category"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot(hist_stat_dowcat)

colnames(alldfs)

#trip station by trip duration cat
hist_stat_dur <- ggplot(alldfs, aes(x = start_station)) + 
  stat_count(aes(fill= duration)) +
  #scale_fill_brewer(palette = "Set2") +
 scale_color_brewer(palette = "BuPu", direction=-1) +
  labs(
    title = "Volume of Bike Trips by Station",
    subtitle = "Fill color: duration",
    x = "Station",
    y = "Volume",
    fill = "duration"
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot(hist_stat_dur)


### station usage
station_use <- table(alldfs$start_station_name)
tail(station_use)
summary(station_use)
max(station_use) #45740
names(which.max(station_use)) #"Philadelphia Museum of Art"
names(which.min(station_use)) #"2nd & Norris"

end_station_use <- table(alldfs$end_station_name)
names(which.max(end_station_use)) #"Philadelphia Museum of Art"
names(which.min(end_station_use)) #""Penn Treaty Park"

colnames(alldfs)

##### READ alldfs
#alldfs <- read.csv(file = '../alldfs.csv') 

#two-way table
#count trips originated from each station by station ID
station_use <- table(alldfs$start_station)
tail(station_use)
summary(station_use)
max(station_use) #210999
min(station_use) #2
names(which.max(station_use))
names(which.min(station_use))
dimnames(station_use)
dim(station_use)
str(station_use)
names(station_use)
class(station_use) #"table"
station_use[["3010"]][1] #45740
station_use[["3226"]][1] #2
station_use[2] #3005 11056 
station_use[[2]] #11056
names(station_use[2]) #3005
names(station_use)[which(station_use == 210999)] #3057

unique(alldfs$start_station) #155
head(alldfs)
dim(station_use) #155

#load indego station table (with coordinates)
stationtable <- read.csv(file = 'miscdata/idgstations.csv') 
stationtable <- na.omit(stationtable)
sum(is.na(stationtable))
dim(stationtable) #164   7
dimnames(stationtable)
tail(stationtable) #4/23/2015 #12/24/2020
lapply(stationtable, class)
#as.Date(stationtable$Go_live_date, tryFormats = "%m/%d/%&")

#extract out station_id and station_name to join with alldfs
stationtable_names <- stationtable %>% select(Station_ID, Station_Name)

#join station names with start_station in alldfs
alldfs <- full_join(stationtable_names, alldfs, by = c("Station_ID" = "start_station"))
colnames(alldfs) 
#rename(.data, new_name = old_name) 
alldfs <- rename(alldfs, start_station_name = Station_Name)
alldfs <- rename(alldfs, start_station = Station_ID)
#alldfs <- within(alldfs, rm(Station_ID))
#join station names with end_station in alldfs
alldfs <- full_join(stationtable_names, alldfs, by = c("Station_ID" = "end_station"))
#rename
alldfs <- rename(alldfs, end_station_name = Station_Name)
alldfs <- rename(alldfs, end_station = Station_ID)
tail(alldfs) 
alldfs <- na.omit(alldfs)
sum(is.na(alldfs))

#write.csv(alldfs, file = "./alldfs.csv")

### PLOT STATIONS
#https://forcats.tidyverse.org/
# plot histogram of station use
#  Reordering a factor by the frequency of values.

#start/end station heatmap


#alldfs <- read.csv(file="./alldfs.csv")

#subset years
df2018 <- subset(alldfs, start_year == 2018, select = c("start_station", "end_station"))
head(df2018)
station_heatmap_tab18 <- table(df2018$start_station, df2018$end_station)
dim(station_heatmap_tab18)
station_heatmap_df18 <- data.frame(station_heatmap_tab18[1:134, 1:134])
names(station_heatmap_df18) <- c("start_station", "end_station", "volume")
max(station_heatmap_df18$volume) #24327
min(station_heatmap_df18$volume) #0
median(station_heatmap_df18$volume) #10
mean(station_heatmap_df18$volume) #43.37297

head(station_heatmap_tab18)
barplot(station_heatmap_tab18)
summary(station_heatmap_df18$volume)
#Min.  1st Qu.   Median     Mean    3rd Qu.     Max. 
#0.00     2.00    10.00    43.37    42.00       24327.00 

alldfs$start_station <- as.factor(alldfs$start_station)
alldfs$end_station <- as.factor(alldfs$end_station)

colnames(alldfs)
#plot volume bar plot
ggplot(data.frame(alldfs), aes(x=start_station)) +
  geom_bar()

ggplot(data.frame(alldfs), aes(x=end_station)) +
  geom_bar()


heatmap2018 <- 
  ggplot(data = station_heatmap_df18, mapping = aes(x=start_station, y=end_station, fill=volume)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "navy", limits=c(0,2000)) +
  labs(
    title = "Heatmap of Trips Volumes Between Stations",
    x = "Starting Station",
    y = "Ending Station",
    fill = "Trip Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_fixed()

heatmap2018


#alldfs station heatmap
station_heatmap_tab <- table(alldfs$start_station, alldfs$end_station)
dim(station_heatmap_tab)
head(station_heatmap_tab)

station_heatmap_df <- data.frame(station_heatmap_tab[1:154, 1:153])
names(station_heatmap_df) <- c("start_station", "end_station", "volume")
head(station_heatmap_df)
tail(station_heatmap_df)
max(station_heatmap_df$volume) #120114
min(station_heatmap_df$volume) #0

station_heatmap_df <- filter(station_heatmap_df, volume != "120114")

gheatmap_base <- 
  ggplot(data = station_heatmap_df, mapping = aes(x=start_station, y=end_station, fill=volume)) +
  geom_tile() + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  #scale_color_viridis(option = "D") +
  labs(
    title = "Heatmap of Trips Between Stations",
    x = "Starting Station",
    y = "Ending Station",
    fill = "Trip Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 10)) + 
  coord_fixed()

gheatmap_base



tail(alldfs)
sum(is.na(alldfs))
alldfs <- na.omit(alldfs)
dim(alldfs) #2311345      24

#check if bike is both standard and electric 
check_bike_type <- intersect(
  alldfs$bike_id[alldfs$bike_type == "standard"], 
  alldfs$bike_id[alldfs$bike_type == "electric"])
check_bike_type #"15067"
#delete bike
alldfs <- filter(alldfs, bike_id != "15067")
length(unique(alldfs$bike_id)) #2769

#number of bikes
length(unique(alldfs$bike_id)) #2769 
bike_use <- table(alldfs$bike_id)
max(bike_use) #2199
min(bike_use) #0
names(bike_use)[which(bike_use == 2199)] #"11170"
bikes <- alldfs %>% select(bike_id, bike_type)
tail(bikes)
dim(bikes)
#remove duplicates
bikes <- bikes[!duplicated(bikes$bike_id),]
dim(bikes) #2769 2


####  LOAD INDEGO STATION INFO CSV
#stationtable <- read.csv(file = 'miscdata/indego-stations-2021-01-01.csv')
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

#https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
#https://gis.stackexchange.com/questions/265339/intersect-coordinates-with-spatial-polygon-with-r

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







