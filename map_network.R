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
d=2

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

#read cluster Ids into the station list
station_list <- station_list %>% left_join(xy, by = "station_id") %>% select(-long,-lat)





### Counting the journeys between clusters ----

#matrix of start and end clusters
df <- data %>% select(start_clust,end_clust) 

# Count the total number of trips made between each station
total_trips <- data.frame(t(apply(df,1,sort))) %>% 
  group_by_all(.) %>% 
  count()

# rename as source and target - although here we're still looking at total trips, so not
# directional
total_trips <- total_trips %>% 
  rename(source = X1, target = X2) 






##### PLOTTING ----

### Visualise the stations on a map of Edinburgh, and the associated clusters ----


# STAMENMAPS ----
# store bounding box coordinates
# edi_bb <- c(left = -3.425166,
#             bottom =  55.890596,
#             right = -3.014034 , 
#             top =  56)
# 
# edinburgh_stamen <- get_stamenmap(bbox = edi_bb,
#                                   zoom = 11, maptype="toner", crop=FALSE)
# map_stamen <- ggmap(edinburgh_stamen, extent = "normal")
# 
# map_clusters_stamen <- map_stamen +
#   #geom_point(data = station_list, aes(station_long, station_lat), alpha = 0.5) +
#   geom_point(data = cent, aes(long_cluster, lat_cluster), colour = "orange", alpha = 0.5, size = 0.1)
# 
# map_clusters_stamen


## SNAZZYMAPS ----
# https://github.com/dr-harper/ggmapstyles

api_key <-  readr::read_csv("api.txt", col_names = FALSE) 
api_key <- api_key$X1

map <- get_snazzymap(center = c(lon = -3.225, lat = 55.95),
                     mapRef = "https://snazzymaps.com/style/28442/monomap",
                     zoom = 11, scale = 2)

(map_snazzy <- ggmap(map))

map_clusters_snazzy <- map_snazzy +
  #geom_point(data = station_list, aes(station_long, station_lat), alpha = 0.5) +
  geom_point(data = cent, aes(long_cluster, lat_cluster), colour = "seagreen", alpha = 0.5, size = 1)

map_clusters_snazzy

# Filter out those trips which are closed loops on the same cluster
total_trips_paths_only <-
  total_trips %>% filter(source != target) %>%
  filter(n >= 100)

col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

# this should be functioned...

# plot edges and nodes
trips_to_plot <- total_trips_paths_only %>% arrange(-n)




map_clusters <- map_clusters_snazzy

map_clusters <- ggplot()
for(i in 1:nrow(trips_to_plot))  {
  
  
 node1 <- cent[cent$clust == trips_to_plot[i,]$source,]
 node2 <- cent[cent$clust == trips_to_plot[i,]$target,]
arc <- rbind(node1,node2) %>% select(lon = long_cluster, lat = lat_cluster)

 # arc <- gcIntermediate( c(node1[1,]$long_cluster, node1[1,]$lat_cluster),
 #                      c(node2[1,]$long_cluster, node2[1,]$lat_cluster),
 #                      n=50, addStartEnd=TRUE ) %>%
 #as_tibble()

 
 
 edge.ind <- ceiling(100*trips_to_plot[i,]$n / max(trips_to_plot$n))
 map_clusters <- map_clusters +  geom_path(data = arc, aes(x = lon, y = lat), col= "black", lwd=.1)

}

map_clusters 

#alternative way of getting 


####  Other graphing stuff -----




# Add cluster centres with a little circle around it
circles_to_plot <- dismo::circles(cent %>% select(-clust), d=d, lonlat = T)

# plot
  plot(circles_to_plot@presence, axes=T)
  plot(xy %>% select(long,lat), add=F)
  

basemap2 + geom_sf(data = circles_to_plot@presence, aes(long_cluster, lat_cluster)) 

# networks
#trips_graph <- as_tbl_graph(total_trips, directed = TRUE)




