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
## Heatmap of Edinburgh Bke Share checkouts in 2020. Those stations with no checkouts in hte latest available week are not included.
## ---------------------------


## Packages
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(cowplot)


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
trips <- trips %>% mutate(month_year = floor_date(date_started,"month")) %>% 
  filter(month_year > as.Date("2019-12-31"))



# Set some theme elements common to the next set of plots
my_theme <- theme(line = element_blank(),
      text = element_blank(),
      title = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = "white") )


cycle_tiles <- trips %>% 
  group_by(start_station_name) %>% 
  filter(any(date_started == max(trips$date_started))) %>% 
  ggplot(aes(x = date_started, y = fct_reorder(start_station_name,total_outward_trips), fill=n_outward_trips)) +
  geom_tile(colour = "white", show.legend = TRUE) +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 12) ,
        title = element_text(size = 12)) +
  xlab("date") +
  labs(title="Edinburgh bike share checkouts by station, 2020",
      subtitle="Counts are for bike checkouts only, and stations with no rentals in the latest available data not shown.",
      caption="Data from https://edinburghcyclehire.com/open-data/historical. Plot by @diarmuidlloyd, but heavily based on @VictimOfMaths COVID-19 heatmaps, available at https://github.com/VictimOfMaths/COVID-19")

cycle_tiles 


cycle_bars <- trips %>% group_by(start_station_name) %>% 
  filter(any(date_started == max(trips$date_started))) %>% 
  summarise(max_n_trips = max(n_outward_trips),  total_outward_trips = max(total_outward_trips))  %>% 
  group_by(start_station_name) %>% 
  ggplot(aes(y =max_n_trips, x = fct_reorder(start_station_name,total_outward_trips), fill = max_n_trips)) +
  geom_col() + 
  coord_flip() +
scale_fill_distiller(palette = "Spectral")  +
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =  element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white") ,
        plot.margin = margin(0, 0, 0, 0, "cm") ) +
  ylab("Maximum hires in a week")

cycle_bars


  
png("output/Edinburgh_weekly_bike_rentals_checkouts_2020.png", units="cm", width=45, height=45, res=500)
plot_grid(cycle_tiles,cycle_bars, align="h", rel_widths=c(1,0.2))
dev.off()
           