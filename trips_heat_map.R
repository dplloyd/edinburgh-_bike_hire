## ---------------------------
##
## Script name: trips_heat_map.R
##
## Purpose of script: Creates heatmap of bike hire usage.
##  
## Author: Diarmuid Lloyd
##
## Date Created: 2020-09-19
##
## Email: diarmuid.lloyd@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## Packages
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)


# Bike trip data, updated daily 
dataPaths <- c(
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/09.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/08.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/07.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/06.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/05.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/04.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/03.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/02.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/01.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/12.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/11.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/10.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/09.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/08.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/07.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/06.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/05.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/04.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/03.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/02.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2019/01.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2018/12.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2018/11.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2018/10.json",
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2018/09.json"
)


#Read in the data. Warning - this file is huge, around 70MB in September 2020.
data <-map_df(dataPaths,fromJSON, flatten = TRUE)
data <- as_tibble()

# Make the file size a bit more manageable
data <- data %>% mutate(started_at = as.Date(started_at) , ended_at = as.Date(ended_at))
#Sort text to factors.
data <- data %>% mutate(start_station_name =  as_factor(start_station_name), end_station_name = as_factor(end_station_name))



#Write the data for local reading - just a convenience if needed down the line
write_csv2(data, "data/cycle_hire_data.csv")

# Select variables of interest.
data_trips <- data %>% select(c("started_at","ended_at","duration","start_station_name","end_station_name"))

# Count up the number of started at trips for each day, plus the total number of trips for each station
trips <- data_trips %>% 
  group_by(start_station_name,date_started = floor_date(started_at, "week")) %>% 
  count(date_started, name = "n_outward_trips") %>% 
  group_by(start_station_name) %>% 
  mutate(total_outward_trips = max(cumsum(n_outward_trips)))


# Add a new variable which has just the month and the year
trips <- trips %>% mutate(month_year = floor_date(date_started,"month"))

cycle_tiles <- trips %>% 
  group_by(start_station_name) %>% 
  filter(any(date_started == max(trips$date_started))) %>% 
  ggplot(aes(x = date_started, y = fct_reorder(start_station_name,total_outward_trips), fill=n_outward_trips)) +
  geom_tile(colour = "white", show.legend = TRUE) +
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  theme_classic()

cycle_tiles 

trips %>% 
  group_by(start_station_name) %>% filter(date_started == max(date_started)) %>% 
  ggplot(aes(x = total_outward_trips, y = fct_reorder(start_station_name,total_outward_trips))) +
  geom_col()
  