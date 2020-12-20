# Created by: Sabi Horvat, December 2020
# www.github.com/wpbsabi , twitter : @tourofdata
# This R script contains Oregon population density maps by county
#   from ten years ago compared to the most recent data

library(tidyverse)  # data wrangling and ggplot2
library(ggrepel)    # helps with labels on plots
library(sf)         # st_read for shape file
library(ggthemes)   # plotting background
library(cowplot)    # plotting multiple plots at once for comparison

### data processing

# population by county
# data : https://sos.oregon.gov/blue-book/Pages/local/county-population.aspx
county_pop <- read_csv('oregon_population_by_county.csv') %>%
  mutate(COUNTY = toupper(County))

# import counties shapefile and merge w/ population data for maps
# http://geog.uoregon.edu/bartlein/courses/geog495/lec06.html 
# county lat long for lables and city lat long for labels/points
counties_ll <- read_csv('oregon_counties_lat_long.csv')
cities <- data.frame(city = c('Portland', 'Salem', 'Eugene'),
                     population = c(653115, 173442, 171245),
                     latitude = c(45.54, 44.92, 44.06),
                     longitude = c(-122.65, -123.02, -123.12))

oregon_shape <- st_read('orcounty.shp') %>% select(NAME)
merge1 <- merge(oregon_shape, county_pop, by.x = 'NAME', by.y = 'County') 
merge2 <- merge(merge1, counties_ll, by.x = 'NAME', by.y = 'County')

### visualization prep

# controlled color-coding
# https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=3
custom_color_scale <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb',
                        '#74a9cf','#3690c0','#0570b0','#045a8d',
                        '#023858')

### visualizations

# population density maps
density2010 <- merge(oregon_shape, county_pop, by.x = 'NAME', by.y = 'County')
density2010$Population <- cut(density2010$'2010',
                              breaks=c(-1,50000,100000,200000,400000,500000,600000,700000,
                                       800000,900000),
                              labels=c("0+","50000+","100000+","200000+","400000+",
                                       "500000+","600000+",
                                       "700000+","800000+"))
plot_density2010 <- density2010 %>% ggplot() +
  geom_sf(aes(fill = Population), color = 'black') +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Oregon Population 2010 (US Census): 3,831,074") +
  coord_sf()  +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust= 10)) +
  scale_fill_manual(values = custom_color_scale) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  

density2018 <- merge(oregon_shape, county_pop, by.x = 'NAME', by.y = 'County')
density2018$Population <- cut(density2018$'2018',
                              breaks=c(-1,50000,100000,200000,400000,500000,600000,700000,
                                       800000,900000),
                              labels=c("0+","50000+","100000+","200000+","400000+",
                                       "500000+","600000+",
                                       "700000+","800000+"))
plot_density2018 <- density2018 %>% ggplot() +
  geom_sf(aes(fill = Population), color = 'black') +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -4,nudge_y = 1,force = 0.5) +
  coord_sf()  +
  theme_void() +
  ggtitle("Oregon Population 2018 (estimated): 4,195,300") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 10)) +
  scale_fill_manual(values = custom_color_scale) + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


population_change <- merge2 %>%
  mutate(change = round(`% Change`*`2018`/100),0)
population_change$`Population Change` <- cut(population_change$change,
                              breaks=c(-50,0,500,1000,5000,10000,30000,
                                       60000,90000,120000),
                              labels=c("-50 to 0","Up to 5000","Up to 1,000",
                                       "Up to 5,000","Up to 10,000",
                                       "Up to 30,000","Up to 60,000",
                                       "Up to 90,000","Up to 120,000"))
plot_change <- population_change %>% ggplot() +
  geom_sf(aes(fill = `Population Change`), color = 'black') +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -4,nudge_y = 1,force = 0.5) +
  coord_sf()  +
  theme_void() +
  ggtitle("Oregon Population Change") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 10)) +
  scale_fill_manual(values = custom_color_scale) + 
  geom_label_repel(data = population_change, aes(Long, Lat, label = change), 
                   force = 0.2, nudge_x = 0, nudge_y = .1,
                   size = 3) + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

# plot multiple plots at once
plot_grid(plot_density2010, plot_density2018, # plot_change,
          labels="Population by Counties in Oregon", vjust=1, hjust=-1)
# plots one at a time
plot_density2010
plot_density2018
plot_change
