## ---------------------------
##
## Script name: ridgeline plots of mean time for rentals
##
## Purpose of script: To compare the mean time of trips for each month.
##
## Author: Diarmuid Lloyd
##
## Date Created: 2020-10-11
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
library(lubridate)
library(cowplot)
library(gt)
library(ggridges)


## Reading data and counting trips -----
data <- read_csv("data/cycle_hire_data.csv")

#First, let's do a test 
data <- data %>% 
  mutate(trip_times = (data$ended_at_full - data$started_at_full) %>% as.numeric() )

data %>% summarise( mean(trip_times) , median(trip_times), sd(trip_times),max(trip_times),min(trip_times))

summary(data$trip_times )

#
sum(data$trip_times >= 600)  
  
data_f <- data %>% filter(trip_times < 90 )

ggplot(data_f)+ geom_histogram(aes(x = trip_times),binwidth = 10)

my_ridgeline <- data_f %>%
  mutate(trip_year = year(started_at_full),trip_month_year = started_at %>% as.Date() %>% format('%m-%Y')) %>% 
  ggplot(aes(x = trip_times, y = as_factor(trip_year), fill = stat(x))) +
  geom_density_ridges_gradient()+
  scale_fill_viridis_c(name = "Trip length (min)", option = "viridis") 


png("output/Density_gradient_test.png",width = 480, height = 480, units = "px")
my_ridgeline
dev.off()               
             