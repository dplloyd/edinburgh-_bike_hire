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
library(gganimate)



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

# Determine the date of the first trip from each station
station_first_trip <-
  data %>% 
  group_by(start_station_id) %>% 
  arrange(started_at) %>% 
  distinct(start_station_id, .keep_all = TRUE) %>%
  select(station_id = start_station_id, first_trip_date = started_at)

#match this in to station_list
station_list <- station_list %>% left_join(station_first_trip, by = "station_id")

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
total_trips_tmp <- df %>% 
  group_by(start_station_id,end_station_id) %>% 
  count() 

# rename as source and target - although here we're still looking at total trips, so not
# directional
total_trips_tmp <- total_trips_tmp %>% 
  rename(source = start_station_id, target = end_station_id) 

#graph <- graph_from_data_frame(total_trips, directed=FALSE) 
# create levels based on all station names. Sort these in order of first appearance
station_id_level <- station_list %>%   arrange(first_trip_date) %>% unique() %>% select(station_id) %>% pull(station_id)
# Adjust the 'to' and 'from' factor levels so they are equal
# to this complete list of node names
total_trips_tmp2 <- total_trips_tmp %>% mutate(source = factor(source, levels = station_id_level),
                       target = factor(target, levels = station_id_level))

return(total_trips_tmp2)
}


# Calculate total trips on all data held
total_trips <- count_trips(data, start = "start_station_id", end =  "end_station_id")
# get the highest number of trips made between stations
maxtrips <- max(total_trips$n)

# Function to filter data, count trips, and plot before writing
write_plot <- function(data,start,end,date_thresh){
  total_trips <-   count_trips(data, start , end )
  
  plot_to_save <- ggplot(total_trips, aes(x = source, y = target, fill = log(n))) +
    geom_raster() +
    theme_minimal() +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) + # to ensure all factors are shown.
    theme( axis.text = element_blank(),
           aspect.ratio = 1) +
    scale_fill_viridis_c(limits = c(0, log(maxtrips)), option = "D")
  
  png(paste0("output/geom_raster_plots/bike_trips_raster_up_to_",date_thresh,".png"))
  print(plot_to_save)
  dev.off()
  
}


# Visualising the trips between stations

ggplot(total_trips, aes(x = source, y = target, fill =(n))) +
  geom_raster() +
  theme_minimal() +
  theme( axis.text = element_blank(),
         aspect.ratio = 1) +
  scale_fill_viridis_c()
  



#Building frames

date_thresh_list <- tibble(dates = seq.Date(from = as.Date("2018-10-01"), to = as.Date("2021-01-31"), by = "month"),
                           date_thresh_reference = as.character(seq(from = 1, by = 1, length.out = length(dates))))



all_data <-
  lapply(
    date_thresh_list$dates,
    FUN = function(x)
      write_plot(data %>% filter(as.Date(started_at) <= x) , start = "start_station_id", end =  "end_station_id",date_thresh = x)
  )

# all_data_unpacked <- bind_rows(all_data, .id = "date_thresh_reference") %>% left_join(date_thresh_list, by = "date_thresh_reference")

# 
# plot_to_animate <- ggplot(all_data_unpacked, aes(x = source, y = target, fill = n)) +
#   geom_raster() +
#   theme_minimal() +
#   theme( axis.text = element_blank(),
#          aspect.ratio = 1) +
#   scale_fill_viridis_c()
# 
# anim <- plot_to_animate +
#   transition_states(dates,
#                     transition_length = 2,
#                     state_length = 1) +
#   ease_aes('cubic-in-out') +
#   ggtitle('{closest_state}')  +
#   exit_disappear() +
#   enter_appear()
# 
# anim2 <- animate(anim, nframes = length(date_thresh_list$dates)+70, end_pause = 2
#                  )
# anim2
