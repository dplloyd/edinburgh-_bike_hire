# Script to test bicycle stations were in areas with lower multiple deprivation quantiles.

library(tidyverse)
library(sf)


# read in all longitude and latitude of stations\

stations <- read.csv(file = "data/cycle_hire_data.csv") %>% 
  select(start_station_name, end_station_name, start_station_latitude,end_station_latitude,
         start_station_longitude, end_station_longitude)

glimpse(stations)


stations_start <- stations %>% 
  select(station_name = start_station_name,lat = start_station_latitude,long = start_station_longitude) %>% 
  distinct()

stations_end <- stations %>% 
  select(station_name = end_station_name,lat = end_station_latitude,long = end_station_longitude) %>% 
  distinct()

stations_long = rbind(stations_start,stations_end) %>% distinct() %>% filter(lat>=54)

shp <- read_sf(dsn = "SG_SIMD_2020", layer = "SG_SIMD_2020")

shp_edi <- shp %>% 
  filter(LAName == "City of Edinburgh")

# We need to make the list of longitude and latitude coordinates a SPATIAL dataset, rather than a geographical one,
# which we do using st_as_sf. We define the CRS as the bog standard one.
stations_long_sf <- stations_long %>%  st_as_sf(coords = c("long","lat"), crs = st_crs("EPSG:4326")) 

# set the shapeful CRS to match that of the spatial dataset
shp_edi_trans <- shp_edi %>% st_transform( crs = st_crs(stations_long_sf) ) %>% 
  mutate(Decilev2_d = as.factor(Decilev2))


simd <- ggplot(shp_edi_trans) + 
  geom_sf(aes(fill=Decilev2_d),colour="white",lwd=0.1) +
  ggtitle("Edinburgh SIMD 2020 Map") +
  coord_sf(datum = NA) +
  scale_fill_viridis_d(alpha = 0.8) # Use viridis pallete - good for colourblindness!
# removes graticules (must be called after geom_sf())
simd

cycleHire <- simd +
  geom_sf(data=stations_long_sf, size = 0.5) +
  labs(title="Edinburgh SIMD 2020 and Just Eat Cycle Hire points",
       caption =  "© Crown copyright and database right 2020",
       fill = "2020 SIMD Decile (v2)") +
  coord_sf(datum = NA) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank() )
  
cycleHire


# I'm interested in what the SIMD Decile of each station is in.
# See for resolving an issue I had:
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

st_crs(stations_long_sf)
st_crs(shp_edi_trans)

sf::sf_use_s2(FALSE)
stations_and_simd <- stations_long_sf %>% st_join(shp_edi_trans, left = TRUE)

SIMD_Decile_count <- shp_edi %>% group_by(Decilev2) %>% 
  count() %>% as_tibble() %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(type = "Datazone")

station_and_simd_count <- stations_and_simd %>% group_by(Decilev2) %>% 
  count() %>% as_tibble() %>% filter(!is.na(Decilev2))%>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(type = "Bike station")

count_combined = rbind(SIMD_Decile_count,station_and_simd_count)

(plot_simd <- count_combined %>% ggplot() +
  geom_col(aes(x = Decilev2, y = prop, fill = type),alpha = 0.9, colour = 'black',
           position = position_dodge2(preserve = "single")) +
  scale_fill_viridis_d() +
  theme_minimal() +
    
    scale_x_continuous(name  = "SIMD Decile",
                        breaks = c(1:10),
                        labels = c(1:10)) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1) ,
                       name = "Proportion") +
  
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        ) +
    
    labs(title = "SIMD 2020 Decile distribution for Edinburgh datazones \nand Just Eat bicycle hire stations")
  
)


count_combined_wide <-count_combined %>% select(type, Decilev2, n) %>% pivot_wider(names_from = type, values_from = n) %>%
  mutate(`Bike station` = ifelse(is.na(`Bike station`), 0, `Bike station`))



res <- chisq.test(count_combined_wide$`Bike station`,p = count_combined_wide$SIMD/ sum(count_combined_wide$SIMD))
res









