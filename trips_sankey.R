## ---------------------------
##
## Script name: trips_sankey.R
##
## Purpose of script: Main purpose is to generate a sankey diagram of trips between stations. Might also add in some other station to ## station analysis.
##
## Author: Diarmuid Lloyd
##
## Date Created: 2020-10-04
##
## Email: diarmuid.lloyd@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## Packages -------
library(tidyverse)
library(magrittr)
library(networkD3)


# Date ranges ----
# Lower and upper cut offs to consider - set as inclusive.
lower_date_cutoff <- as.Date("2020-09-25")
upper_date_cutoff <- Sys.Date()

## Reading data and counting trips -----
data <- read_csv("data/cycle_hire_data.csv")

# Select variables of interest, and filter dates based on start date
data_trips <- data %>% select(c("started_at","ended_at","duration","start_station_name","end_station_name")) %>% 
  filter(started_at >= lower_date_cutoff, ended_at < upper_date_cutoff)

# I know there are some stations grouped together which would be considered the same 'station', but will have a separate entry. It woulkd make sense to group these together.
#So first get a list of all the unique station names from the started_at and ended_at
start <- data_trips %$% unique(start_station_name)
end <- data_trips %$% unique(end_station_name)
all_stations <- c(start, end ) %>% unique()
#Return to this later.

#count up all the unique trip combos
results <- count(data_trips,start_station_name,end_station_name)



## Sankey diagram ----


# format in prep for sankey diagram. We assign a node refernce to each station. Node ID must start at 0.
nodes <- data.frame(node = seq(from = 0, to = length(all_stations)-1), 
                    name = all_stations)



#create links dataframe, which quantifies the flow between each node.
links <- merge(results, nodes, by.x = "start_station_name", by.y = "name") %>%
  rename("source" = "node")
  
  
links <- merge(links, nodes, by.x = "end_station_name", by.y = "name") %>% 
  rename("target" = "node","value" = "n") %>% select(c("value","source","target"))


sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', Target = 'target', Value = 'value')
