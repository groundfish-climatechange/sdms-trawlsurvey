# Plot Atlantis, ROMS, and GLORYS domains
library(tidyverse)
library(sf)
library(rbgm)
library(here)
library(rnaturalearth)

# Coastline
# spatial background map
# load west cost land for mapping
coast <- ne_countries(continent="North America",scale=50,returnclass = 'sf') %>% 
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  st_crop(emocc_sf)

#### Atlantis grid ####
fl <- here::here("data",'grids',"CalCurrentV3_utm.bgm")
# load the file
bgm <- read_bgm(fl)
names(bgm)
# Atlantis sf object
emocc_sf <- box_sf(bgm) %>% dplyr::select(box_id,label,area,botz) %>% 
  st_transform("+proj=utm +zone=10 +datum=WGS84 +units=km")

#### GLORYS ####

# GLORYS temp/oxygen data
glorys <- read_rds(here('data','GLORYS_bottom_temp_o2_SDMs.rds')) %>% 
  # convert to UTM zone 10
  st_as_sf(coords=c('longitude','latitude'),crs=4326) %>% 
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(latitude = sf::st_coordinates(.)[,2],
         longitude = sf::st_coordinates(.)[,1])

# glorys bbox with areas that are too deep removed
glorys_full <- glorys %>% summarise() %>% st_convex_hull()%>% 
  st_difference(coast %>% summarise())

#### ROMS ####
# roms domain (for display purposes)
roms_domain <- read_rds(here::here('data','roms_latlon_depth_key_topleft_start.rds')) %>% 
  mutate(roms_cell=row_number()) %>% 
  st_as_sf(coords=c('lon','lat'),crs=4326) %>%
  summarise() %>% 
  st_convex_hull() %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  st_difference(coast %>% summarise())

atlantis_glorys_extent <- glorys %>% 
  # filter(depth<max_depth) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_difference(coast %>% summarise())

# atlantis polygons, summarized/unioned
emocc_merged <- emocc_sf %>% summarise()
emocc_no_offshore_merged <- emocc_sf %>% 
  filter(botz!=min(botz)) %>% 
  summarise()

# PLOT
# plot our starting point(s)
ggplot()+
  geom_sf(data=coast)+
  geom_sf(data=glorys_full,fill='blue',alpha=0.5)+
  geom_sf(data=emocc_sf,fill='red',alpha=0.5)+
  geom_sf(data=roms_domain,fill='darkgreen',alpha=0.5)

ggplot()+
  geom_sf(data=coast)+
  geom_sf(data=atlantis_glorys_extent,fill='blue',alpha=0.5)+
  geom_sf(data=emocc_no_offshore_merged,fill='red',alpha=0.5)

ggplot()+
  geom_sf(data=coast,fill='gray50')+
  geom_sf(data=emocc_sf,fill="purple",alpha=0.5)+
  coord_sf(datum=NA)+
  theme_minimal()
