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



# Read the raw data which we saved using read_JSON_and_save_locally.R
data <- read_csv("data/cycle_hire_data.csv")

# To build the network map we need:
#  - The number of trips taken between each station
#  - The coordinates of each station
#
# The some stations have multiple docks, but can essentially be thought of as the same place. We should first identify these stations, and create aggregate station locations. We'll just use one of the lat-long positions as a proxy for the whole lot

### Identifying unique stations ------

# remove the Smarter Station, which is somewhere in england
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
  ) %>%
  # keep only the unique rows
  unique()


### Aggregating similar stations together ----

longlat <- tibble(long = station_list$station_long, lat = station_list$station_lat)


# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(longlat$long,longlat$lat), ncol=2), data.frame(station_id =  station_list$station_id),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold
d=50

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)

# calculate the centroid of the clusters
cent <- matrix(ncol=2, nrow=max(xy$clust))
for (i in 1:max(xy$clust)){
  # gCentroid from the rgeos package
  cent[i,] <- rgeos::gCentroid(subset(xy, clust == i))@coords
}

# convert to tibbles. probably a bit short-sighted, but hey ho
cent <- cent %>% as_tibble() %>%  
  rename(long_cluster = V1, lat_cluster = V2) %>% 
  mutate(clust = seq(1,max(xy$clust)))

xy <- xy %>% 
  as_tibble() %>%  
  rename(long = coords.x1, lat = coords.x2)

# Read cluster ids into the main data frame

data <- data %>% 
  # add in the start station cluster number
  left_join( xy, by = c("start_station_id" = "station_id") ) %>% rename(start_clust = clust) %>% select(-long,-lat) %>% 
  # add in the end station cluster number 
  left_join( xy, by = c("end_station_id" = "station_id") ) %>% rename(end_clust = clust) %>% select(-long,-lat) %>% 
  # add in the start cluster center coordinates
  left_join( cent, by = c("start_clust" = "clust") ) %>% rename(start_clust_long = long_cluster, start_clust_lat = lat_cluster) %>% 
  # add in the end cluster center coordinates
  left_join( cent, by = c("end_clust" = "clust") ) %>% rename(end_clust_long = long_cluster, end_clust_lat = lat_cluster) 

### Visualise the stations on a map of Edinburgh, and the associated clusters
# store bounding box coordinates
edi_bb <- c(left = -3.425166,
            bottom =  55.890596,
            right = -3.014034 , 
            top =  55.995832)

  edinburgh_stamen <- get_stamenmap(bbox = edi_bb,
                                zoom = 12, maptype="toner-lite", crop=FALSE)
ggmap(edinburgh_stamen)



### Counting the journeys between clusters



