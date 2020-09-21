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
## Heatmap of Edinburgh Bke Share checkouts in 2020. Those stations with no checkouts in hte latest available week are not included. Barplot s
## shows the peak number of rentals in a week in 2020, and stations sorted in descending order for total bike checkouts in 2020.
##
## This script is heavily based on @VictimOfMaths' COVI19 heatmaps, available here: https://github.com/VictimOfMaths/COVID-19
## ---------------------------


## Packages
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(cowplot)
library(gt)


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

## Run if the first time, or if updating. If not, comment out and read in tehe locally saved csv instead.
#Read in the data. Warning - this file is huge, around 70MB in September 2020.
# tictoc::tic()
# data <-map_df(dataPaths,fromJSON, flatten = TRUE)
# tictoc::toc()
# data <- data %>% as_tibble()
# 
# # Make the file size a bit more manageable
# data <- data %>% mutate(started_at = as.Date(started_at) , ended_at = as.Date(ended_at))
# #Sort text to factors.
# data <- data %>% mutate(start_station_name =  as_factor(start_station_name), end_station_name = as_factor(end_station_name))
# 
# #Write the data for local reading - just a convenience if needed down the line
# write_csv(data, "data/cycle_hire_data.csv")


data <- read_csv("data/cycle_hire_data.csv")

# Select variables of interest.
data_trips <- data %>% select(c("started_at","ended_at","duration","start_station_name","end_station_name"))

# Count up the number of started at trips for each day, plus the total number of trips for each station
trips <- data_trips %>% 
  group_by(start_station_name,date_started = floor_date(started_at, "week")) %>% 
  count(date_started, name = "n_outward_trips") %>% 
  group_by(start_station_name) %>% 
  mutate(total_outward_trips = max(cumsum(n_outward_trips)))

#create the heatmap. 

# I want implicit missing values to be explicitly missing, so that geom_tile below can show a colour for 0 cases.
full_date_range <- seq.Date(as.Date("2020-01-05"),as.Date("2020-09-13"),by = 7)

trips <- trips %>% 
  group_by(start_station_name) %>% 
  complete(date_started = full_date_range)

# Add a new variable which has just the month and the year
trips <- trips %>% mutate(month_year = floor_date(date_started,"month")) %>% 
  filter(month_year > as.Date("2019-12-31"))

trips %>% group_by(start_station_name) %>% 
  filter(any((date_started == max(trips$date_started)) & (!is.na(date_started == TRUE) )))
  
#this results in total_outward_trips being NA for the missing dates, whcih causes problems down the line. So,
# set the NAs to the station's total trips
trips <-  trips %>% group_by(start_station_name) %>% 
  mutate(total_outward_trips = max(total_outward_trips, na.rm = TRUE))


colour_limit <- c(1,400)


cycle_tiles <- trips %>% 
  group_by(start_station_name) %>% 
  filter(any( date_started == max(trips$date_started) & !is.na(n_outward_trips == TRUE) )) %>% 
  ggplot(aes(x = date_started, y = fct_reorder(start_station_name,total_outward_trips), fill= n_outward_trips)) +
  geom_tile(colour = "white", show.legend = TRUE) +
  scale_fill_distiller(palette = "Spectral",na.value = "gray95", limits =c(1,max(trips$n_outward_trips,na.rm = TRUE))) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "month") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black"),
        legend.position = "none",
        axis.text.y = element_text(size = 12) ,
      plot.caption = element_text(size = 12),
      panel.background = element_rect(fill = "white")) +
  xlab("Date") +
  labs(title="Edinburgh bike share rentals by station, 2020",
      subtitle="Counts are for bike checkouts only, and stations with no rentals in the latest available data not shown. Gray gaps are where no rentals recorded.",
      caption="Data from https://edinburghcyclehire.com/open-data/historical. Plot by @diarmuidlloyd, but heavily based on @VictimOfMaths' COVID-19 heatmaps (https://github.com/VictimOfMaths/COVID-19)")

cycle_tiles 

# create barplot for peak bike rentals in a week for 2020
cycle_bars <- trips %>% 
  group_by(start_station_name) %>% 
  filter(any( date_started == max(trips$date_started) & !is.na(n_outward_trips == TRUE) )) %>% 
  summarise(max_n_trips = max(n_outward_trips, na.rm = TRUE),  total_outward_trips = max(total_outward_trips))  %>% 
  group_by(start_station_name) %>% 
  ggplot(aes(y =max_n_trips, x = fct_reorder(start_station_name,total_outward_trips), fill = max_n_trips)) +
  geom_col() + 
  coord_flip() +
scale_fill_distiller(palette = "Spectral")  +
  scale_y_continuous(breaks = pretty(trips$total_outward_trips,n=100) )+
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =  element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white") ,
        plot.margin = margin(0, 0, 0, 0, "cm") ) +
  ylab("Peak weekly hire in 2020")

cycle_bars

# write the plot
png("output/Edinburgh_weekly_bike_rentals_checkouts_2020.png", units="cm", width=45, height=45, res=500)
montage <- plot_grid(cycle_tiles,cycle_bars, align="h", rel_widths=c(1,0.2))
montage
dev.off()
           

# Some clear peaks of use, most apparent in the Portobello Kings Road station.
data %>% filter(start_station_name == "Portobello - Kings Road") %>% 
  group_by(started_at) %>% count(started_at) %>% 
  ggplot() + geom_line(aes(x = started_at, y = n)) 

# Identifying the days in 2020 which had the most peak use of stations.
common_peak_dates <- data_trips %>% 
  group_by(start_station_name) %>% count(started_at) %>% 
filter(n ==max(n),started_at >= "2020-01-01") %>% 
  ungroup() %>% 
  count(started_at) %>% 
  arrange(desc(n))




# Most common day where stations recorded their peak use in 2020 so far.
common_peak_dates %>% gt(rowname_col = "row") %>% cols_label(
  started_at = "Date",
  n = "Stations"
)
                              

