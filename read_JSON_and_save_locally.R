## ---------------------------
##
## Script name: read_JSON_and_save_locally.R
##
## Purpose of script: Creates a local version of hte most recent JSON data available on Just Eat website. This is faster than reading ## the data from justeat each time I run my analysis/data visualtion scripts
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
#--------


library(tidyverse)
library(readr)
library(jsonlite)
library(lubridate)

# Paths to bike trip data, updated daily. Each month a new line should
# be added.
dataPaths <- c("https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/10.json", 
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
               "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2018/09.json")


## Read JSON files ------ Read in the data. Warning - this file is huge,
## around 70MB in September 2020. Takes around 50 seconds on my machine
tictoc::tic()
data1 <- map_df(dataPaths, fromJSON, flatten = TRUE)
tictoc::toc()
data <- data1 %>% as_tibble()

# Make the file size a bit more manageable
data <- data %>% mutate(started_at_full = ymd_hms(started_at), ended_at_full = ymd_hms(ended_at), 
                        started_at = as.Date(started_at), ended_at = as.Date(ended_at)) %>% 
  # Sort text to factors.
  data <- data %>% mutate(start_station_name = as_factor(start_station_name), 
                          end_station_name = as_factor(end_station_name))

# Write the data for local reading
write_csv(data, "data/cycle_hire_data.csv")

