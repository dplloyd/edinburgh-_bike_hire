## ---------------------------
##
## Script name: 
##
## Purpose of script: To prepare bike hire data for Shiny
##
## Author: Diarmuid Lloyd
##
## Date Created: 2020-12-27
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
## Packages
library(tidyverse)
library(lubridate)
library(janitor)
library(forcats)


## Reading data and counting trips -----
data <- read_csv("data/cycle_hire_data.csv") %>% as_tibble()



# Determine all levels
all_station_names <- c(
  unique(data$start_station_name),
  unique(data$end_station_name)
) %>% unique()

data <- data %>% mutate(start_station_f = factor(start_station_name, all_station_names) ,
                        end_station_f = factor(start_station_name, all_station_names) )



# Collapse certain stations together


#For the Shiny app, I want to create aggregate data sets. Create a total, tooß
agg_started_at <- data %>% ungroup() %>% 
  mutate(started_at_day = as.Date(started_at)) %>%
  group_by(start_station_f,started_at_day) %>% 
  summarise(total_started_at = n()) %>% 
  adorn_totals()


ggplot(data) + geom_col(  aes(x = ))


write_csv(agg_started_at,"output/agg_started_at.csv")
