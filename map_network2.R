## ---------------------------
##
## Script name: map_network.R
##
## Purpose of script: Visualise journeys taken between bike hire stations
##
## Author: Diarmuid Lloyd
##
## Date Created: 2021-02-07
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
library(readr)
library(sp)
library(rgdal)
library(geosphere)
library(ggmap)
library(igraph)
library(ggmapstyles)



# Read the raw data which we saved using read_JSON_and_save_locally.R
data <- read_csv("data/cycle_hire_data.csv")

# To build the network map we need:
#  - The number of trips taken between each station
#  - The coordinates of each station
#
# The some stations have multiple docks, but can essentially be thought of as the same place. We should first identify these stations, and create aggregate station locations. We'll just use one of the lat-long positions as a proxy for the whole lot

### Identifying unique stations ------

# remove the Smarter Station, which is somewhere in England
data <- data %>% filter(start_station_id != 280, end_station_id != 280)

# Row bind the list of all start stations and end stations, and keep only unique rows
station_list <-
  # combine the start and end station location and identifier information
  rbind(
    data %>%
      # grab all the start station identifier info
      select(
        station_id = start_station_id,
        station_name = start_station_name,
        station_long =  start_station_longitude,
        station_lat = start_station_latitude
      ),
    data %>%
      # grab all the end station identifier info
      select(
        station_id = end_station_id,
        station_name = end_station_name,
        station_long = end_station_longitude,
        station_lat =  end_station_latitude
      )
  ) 
  # keep only the unique rows
station_list <- station_list[ !duplicated(station_list$station_id),]

### Aggregating similar stations together ----

longlat <- tibble(long = station_list$station_long, lat = station_list$station_lat)


# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(longlat$long,longlat$lat), ncol=2), data.frame(station_id =  station_list$station_id),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)


### Counting the journeys between stations ----

# Function counts all trips between start and end stations
count_trips <- function(data, start, end){
# start = name of variable with start node
# end: name of variable with end node
  df <-
    data %>% select(start_station_id = start,
                    end_station_id = end)  

# Count the total number of trips made between each station, direction matters here
total_trips <- df %>% 
  group_by(start_station_id,end_station_id) %>% 
  count() 

# rename as source and target - although here we're still looking at total trips, so not
# directional
total_trips <- total_trips %>% 
  rename(source = start_station_id, target = end_station_id) 

#graph <- graph_from_data_frame(total_trips, directed=FALSE) 
# create levels based on all station names. Sort these in order of first appearence
station_id_level <-  sort(station_list$station_id) %>% unique()
# Adjust the 'to' and 'from' factor levels so they are equal
# to this complete list of node names
total_trips <- total_trips %>% mutate(source = factor(source, levels = station_id_level),
                       target = factor(target, levels = station_id_level))

return(total_trips)
}

total_trips <- count_trips(data, "start_station_id", "end_station_id")


# Visualising the trips between stations

ggplot(total_trips, aes(x = source, y = target, fill =log(n))) +
  geom_raster() +
  theme_bw() +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none") 

