# Small script to set the domain for SDMs and SDM projections using ROMS
# We will use the maximal extent of the trawl survey, and crop it with the coastline
library(tidyverse)
library(sf)
library(rnaturalearth)
library(here)

# ROMS extent
roms_bbox <- read_rds(here::here('data','roms_latlon_key.rds')) %>% 
  mutate(roms_cell=row_number()) %>% 
  st_as_sf(coords=c('lon','lat'),crs=4326) %>%
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km")

# trawl extent
# from Eric Ward
# trawl_bbox <- read_csv(here::here('data','trawl','trawl_survey_locations.csv')) %>% 
#   st_as_sf(coords=c('longitude_dd','latitude_dd'),crs=4326) %>%
#   summarise() %>% 
#   st_convex_hull() %>% 
#   st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km")

gr <- read_rds(here::here('data','grids','wc_grid.rds'))
# bounding box for trawl grid
gr_sp <- gr %>% st_as_sf(coords=c('X','Y'),crs="+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  summarise() %>%
  st_convex_hull()

#cowcod conservation areas
CCA = read_sf(here('data','grids','kv299cy7357.shp')) %>% st_transform("+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  st_union()

# crop CCA out of trawl bounding box
trawl_bbox <- gr_sp %>% st_difference(CCA)

# coastline
coast <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km")

# do the cropping
# uncropped
ggplot()+
  geom_sf(data=coast)+
  geom_sf(data=roms_bbox,fill='blue',alpha=0.5)+
  geom_sf(data=trawl_bbox,fill='red',alpha=0.5)

# to make the correct domain, crop trawl to coastline
out <- trawl_bbox %>% 
  st_difference(coast %>% summarise())

ggplot()+
  geom_sf(data=coast)+
  geom_sf(data=out,fill='red',alpha=0.5)+
  labs(title="Domain for Projected SDMs")

# save the outline
write_rds(out,here::here('data','cropped_domain_for_projected_sdms.rds'))
