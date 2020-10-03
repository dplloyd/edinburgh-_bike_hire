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
#--------


## Packages
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(cowplot)
library(gt)


# Bike trip data, updated daily 
dataPaths <- c(
  "https://data.urbansharing.com/edinburghcyclehire.com/trips/v1/2020/10.json",
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

# Date ranges ----
lower_date_cutoff <- as.Date("2019-12-31")


## OPTIONAL ------
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

## Reading data and counting trips -----
data <- read_csv("data/cycle_hire_data.csv")

# Select variables of interest.
data_trips <- data %>% select(c("started_at","ended_at","duration","start_station_name","end_station_name"))

# Count up the number of started at trips for each day, plus the total number of trips for each station
trips <- data_trips %>% 
  group_by(start_station_name) %>% 
  mutate(trip_week = floor_date(started_at, "week")) %>% 
  count(trip_week, name = "n_outward_trips") %>% 
  group_by(start_station_name) %>% 
  mutate(total_outward_trips = max(cumsum(n_outward_trips)))

#create the heatmap. 

# I want implicit missing values to be explicitly missing, so that geom_tile can show a gray tile for NA cases, rather than just an empty tile.
full_date_range <- seq.Date(as.Date(min(trips$trip_week)),as.Date(max(trips$trip_week)),by = 7)

trips <- trips %>% 
  group_by(start_station_name) %>% 
  complete(trip_week = full_date_range)

# Filter date range of interest.
trips <- trips %>%
filter(trip_week > lower_date_cutoff)
  
#this results in total_outward_trips being NA for the missing dates, whcih causes problems down the line. So,
# set the NAs to the station's total trips
trips <-  trips %>% group_by(start_station_name) %>% 
  mutate(total_outward_trips = max(total_outward_trips, na.rm = TRUE))


# I want to know the sum total of trips each week.
total_trips <- trips %>% ungroup() %>%  group_by(trip_week) %>% summarise(n_outward_trips =sum(n_outward_trips,na.rm = TRUE)) %>% mutate(start_station_name = "Total",total_outward_trips = sum(n_outward_trips))

# Add to the working dataset
trips_to_plot <- bind_rows(trips,total_trips) %>% mutate(start_station_name = as_factor(start_station_name))

# remove the stations which haven't had a rental in the latest week of data available.
trips_to_plot <- trips_to_plot %>% 
  group_by(start_station_name) %>% 
  filter(any( trip_week == max(trips$trip_week) & !is.na(n_outward_trips == TRUE) )) 





# Heatmap  - station counts only ------------------------------------------


## Heatmap of weekly total trips, with no total row 
cycle_tiles <- trips_to_plot %>% filter(start_station_name != "Total") %>% 
  ggplot(aes(x = trip_week, y = fct_reorder(start_station_name,total_outward_trips), fill= n_outward_trips)) +
  geom_tile(colour = "white", show.legend = TRUE) +
  scale_fill_distiller(palette = "Spectral",na.value = "gray95", limits =c(1,max(trips$n_outward_trips,na.rm = TRUE))) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "month",sec.axis = dup_axis()) +
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



# Barplot - peak rental value in time series  -----------------------------


# create barplot for peak bike rentals in a week for 2020
cycle_bars <- trips_to_plot %>% 
  summarise(max_n_trips = max(n_outward_trips, na.rm = TRUE),  total_outward_trips = max(total_outward_trips))  %>% 
  group_by(start_station_name) %>% 
  filter( start_station_name!="Total") %>% 
  ggplot(aes(y =max_n_trips, x = fct_reorder(start_station_name,total_outward_trips), fill = max_n_trips)) +
  geom_col() + 
  coord_flip() +
  scale_fill_distiller(palette = "Spectral")  +
  scale_y_continuous(breaks = pretty(trips$total_outward_trips,n=100),sec.axis = dup_axis() )+
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =  element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white") ,
        plot.margin = margin(0, 0, 0, 0, "cm") ) +
  ylab("Peak weekly hire in 2020")

cycle_bars





# Heatmap - as % of peak weekly count, inc total --------------------------

# Setting a sensible colour scale, based on the extent of deviation from the time series
trips_to_plot <- trips_to_plot %>% group_by(start_station_name) %>% 
  mutate( prop_of_weekly_max  = n_outward_trips / max(n_outward_trips,na.rm = TRUE),
          prop_of_station_total = n_outward_trips / total_outward_trips)

## Heatmap of weekly total trips, with no total row 
cycle_tiles_peakprop <- trips_to_plot %>% 
  ggplot(aes(x = trip_week, y = fct_reorder(start_station_name,total_outward_trips), fill= prop_of_weekly_max)) +
  geom_tile(colour = "white", show.legend = TRUE) +
  scale_fill_distiller(palette = "Spectral",na.value = "gray95") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "month",sec.axis = dup_axis()) +
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
       subtitle="Bike share check-outs as a proportion of each station's peak weekly total. Stations with no rentals in the latest available data not shown. Gray gaps are where no rentals recorded.",
       caption="Data from https://edinburghcyclehire.com/open-data/historical. Plot by @diarmuidlloyd, but heavily based on @VictimOfMaths' COVID-19 heatmaps (https://github.com/VictimOfMaths/COVID-19)")

cycle_tiles_peakprop 



# Barplot - peak rental value in time series, total of all stations set to zero   -----------------------------

cycle_bars_total_zero <- trips_to_plot %>% 
  mutate(total_outward_trips_adj = ifelse(start_station_name == "Total",0,total_outward_trips)) %>% 
  summarise(max_n_trips = max(n_outward_trips, na.rm = TRUE),  
            total_outward_trips_adj = max(total_outward_trips_adj),
            total_outward_trips = max(total_outward_trips))  %>% 
  group_by(start_station_name) %>% 
  ggplot(aes(y =max_n_trips, x = fct_reorder(start_station_name,total_outward_trips), fill = log10(max_n_trips))) +
  geom_col() + 
  coord_flip() +
  scale_fill_distiller(palette = "Spectral")  +
  scale_y_continuous(trans = "log10" ,sec.axis = dup_axis())+
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =  element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white") ,
        plot.margin = margin(0, 0, 0, 0, "cm") ) +
  ylab("Peak weekly hire in 2020")

cycle_bars_total_zero



## Write the plots -------------------------------------------

png("output/Edinburgh_weekly_bike_rentals_checkouts_2020.png", units="cm", width=45, height=45, res=500)
montage <- plot_grid(cycle_tiles,cycle_bars, align="h", rel_widths=c(1,0.2))
montage
dev.off()


png("output/Edinburgh_weekly_bike_rentals_checkouts_peak_proportion_2020.png", units="cm", width=45, height=45, res=500)
montage <- plot_grid(cycle_tiles_peakprop,cycle_bars_total_zero, align="h", rel_widths=c(1,0.2))
montage
dev.off()



## Sanity check -----

# Check some gaps. Not sure they are correct.
trips_to_plot %>% filter(start_station_name == "Cramond Foreshore", trip_week > "2020-02-01") %>% filter(is.na(n_outward_trips) == TRUE)
#so 16-02-2020 week is NA, implying there were no rentals in that period.

#so let's check the raw data for this time
data_trips %>% filter(start_station_name == "Cramond Foreshore", started_at > "2020-02-01", started_at < "2020-02-28")
# And looks like there were indeed no rentals in that time period.



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
                              

