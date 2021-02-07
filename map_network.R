## ---------------------------
##
## Script name: map_network.R
##
## Purpose of script: Visalise journeys taken between bke hire stations
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
library(geodist)


# Read the raw data which we saved using read_JSON_and_save_locally.R
data <- read_csv("data/cycle_hire_data.csv")

# To build the network map we need:
#  - The number of trips taken between each station
#  - The coordinates of each station
#
# The some stations have multiple docks, but can essentially be thought of as the same place. We should first identify these stations, and create aggregate station locations. We'll just use one of the lat-long positions as a proxy for the whole lot

### Identifying unique stations ------

# Row bind the list of all start stations and end stations, and keep only unique rows
station_list <-
  rbind(
    data %>% select(
      station_id = start_station_id,
      station_name = start_station_name,
      station_long =  start_station_longitude,
      station_lat = start_station_latitude
    ),
    data %>% select(
      station_id = end_station_id,
      station_name = end_station_name,
      station_long = end_station_longitude,
      station_lat =  end_station_latitude
    )
  ) %>%
  unique()



### Aggregating similar stations together ----
# The crudest way is to sort the data frame by name, and check for those name station A, B C etc, (or similar)
station_list <- station_list %>% arrange(by_group = station_name)
# From a quick check, there are more stations than I anticipated which are "split".
# pairwise_distances <-
#   dist(cbind(station_list$station_long, station_list$station_lat)) %>% as.matrix() %>% as_tibble()

longlat <- tibble(long = station_list$station_long, lat = station_list$station_lat)

pairwise_distances <- geodist( x = longlat , measure = "haversine") %>% as.matrix() %>% as_tibble()

number_of_stations <- nrow(station_list)

colnames(pairwise_distances) <- station_list$station_name

# This chain starts with the pairwise distances, converts to long=form, while adding in a couple of
# useful variables, and filtering out all those which are the same station.
pairwise_distances_long <-
  pairwise_distances %>% pivot_longer(cols = everything(),
                                      names_to = "station1",
                                      values_to = "distance") %>%
  mutate(
    station2 = rep(station_list$station_name, each = number_of_stations, times = 1),
    station1_id = rep(station_list$station_id,
                      each = 1,
                      times = number_of_stations),
    station2_id = rep(station_list$station_id, each = number_of_stations, times = 1)
  ) %>% 
  slice_head(n = number_of_stations^2 / 2 ) %>% 
  arrange(by_group = distance) %>%
  filter(station1_id != station2_id) 

ggplot(pairwise_distances_long) + geom_line(aes(x = seq(1,length(distance)), y = distance))


