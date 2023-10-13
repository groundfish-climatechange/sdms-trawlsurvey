# Organize DTS footprints
# Footprints provided by Becca Selden
# Updated 08/15/2022

library(tidyverse)
library(sf)
library(rnaturalearth)
library(here)
library(ggsci)

# import the footprints

shp_fls <- list.files(here('data','DTS footprints'),full.names = T) %>% str_subset(".shp") %>% str_subset("combined",negate = T)

footprints <- purrr::map(shp_fls,read_sf) %>% 
  bind_rows() %>% 
  dplyr::select(-id) %>% 
  mutate(port_group=c('Astoria','Brookings','Coos Bay',"Crescent City","Eureka","Fort Bragg","Los Angeles",
                      "Monterey","Morro Bay","Newport","Puget Sound","San Diego","San Franscisco","Santa Barbara","WA Coast"))
bb <- st_bbox(footprints)

coast <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%
  st_transform(st_crs(footprints))

ggplot()+
  geom_sf(data=coast,fill='grey70')+
  geom_sf(data=footprints,aes(fill=port_group),alpha=0.6)+
  xlim(bb[1],bb[3])+ylim(bb[2],bb[4])+
  coord_sf(datum=NA)+
  theme_void()+
  scale_fill_discrete(name="Port Group")

# write as one file
write_sf(footprints,here('data','DTS footprints','DTS_footprints_combined.shp'))
