# Overlay Atlantis polygons with IOPAC port-level fish footprints from Jameal/Becca
# 04.29.2022
# UPDATE 05.20.2022: adding tribal usual and accustomed areas

library(tidyverse)
library(rbgm)
library(here)
library(rnaturalearth)
library(sf)
library(units)
select <- dplyr::select

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

options(dplyr.summarise.inform = FALSE)

# load west cost land for mapping
coaststates <- ne_states(country='United States of America',returnclass = 'sf') %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada')) %>%
  st_transform(st_crs(fp))
# import Atlantis bgm as sf
# load the file
bgm <- read_bgm(here('data','grids','CalCurrentV3_utm.bgm'))
names(bgm)
atlantis_sf <- box_sf(bgm) %>% select(label,.bx0,box_id,poly_area=area)
st_crs(atlantis_sf) <- st_crs(attr(atlantis_sf$geometry, "crs")$proj)
# transform to same crs as the footprints
atlantis_sf <- atlantis_sf %>% st_transform(st_crs(fp))

# import footprints
fp <- read_sf(here('data','atlantis','logbook_all_pvcs.shp'))
## filter footprints to just the most recent period (period 4, catch shares)
fp4 <- fp %>% filter(period=="4_Catch_Shares")

# tribal UAs
tribes <- read_sf(here('data','atlantis','tribal_U&As_with_700fm_cutoff.shp')) %>% 
  st_transform(st_crs(fp))
# IMPORTANT UPDATE: MAKAH ARE THE ONLY TRIBE THAT USE TRAWL
# so, tribal trawl will use the Makah footprint
# tribal non-trawl will use the combined (all-tribe U&As) footprints
tribes_trawl_fp <- tribes %>% 
  filter(UA=="Makah")
tribes_nontrawl_fp <- tribes %>% 
  summarise() %>% 
  mutate(UA="all tribes")
tribes_trawl_nontrawl <- bind_rows(tribes_trawl_fp,tribes_nontrawl_fp) %>% 
  mutate(fleet=c("Tribal trawl","Tribal non-trawl"))
tbbox=st_bbox(tribes_trawl_nontrawl)
ggplot()+
  geom_sf(data=coaststates)+
  geom_sf(data=tribes_trawl_nontrawl,aes(fill=fleet,linetype=fleet))+
  coord_sf(xlim=c(tbbox[1],tbbox[3]),ylim=c(tbbox[2],tbbox[4]))+
  facet_wrap(~fleet)

# plot our starting point
bbox <- st_bbox(atlantis_sf)
ggplot()+
  geom_sf(data=coaststates)+
  geom_sf(data=atlantis_sf,fill='lightblue')+
  geom_sf(data=fp4,aes(fill=port_name))+
  geom_sf(data=tribes_trawl_nontrawl,aes(fill=fleet))+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))

# do spatial overlay
fp_atlantis_join <- fp4 %>% 
  st_intersection(atlantis_sf) %>% 
  # calculate area of each individual footprint/polygon overlap
  mutate(area_overlap=st_area(.)) %>% 
  # then, calculate each of these areas as a proportion of total polygon area
  mutate(prop_poly=area_overlap/poly_area) %>% 
  mutate(prop_poly=as.numeric(prop_poly))

tribes_atlantis_join <- tribes_trawl_nontrawl %>% 
  st_intersection(atlantis_sf) %>% 
  # calculate area of each individual footprint/polygon overlap
  mutate(area_overlap=st_area(.)) %>% 
  # then, calculate each of these areas as a proportion of total polygon area
  mutate(prop_poly=area_overlap/poly_area) %>% 
  mutate(prop_poly=as.numeric(prop_poly)) %>% 
  # group by tribe and poly and summarise
  group_by(fleet,box_id) %>% 
  summarise(prop_poly=sum(prop_poly)) %>% 
  ungroup()

glimpse(fp_atlantis_join)
glimpse(tribes_atlantis_join)

# arrange final output as full data frames (port by polygon by proportion)
fp_prop_polys<- fp_atlantis_join %>% 
  mutate(prop_poly=round(prop_poly,digits=6)) %>% 
  st_set_geometry(NULL) %>% 
  select(port_name,box_id,prop_poly) %>% 
  #fill in zeroes for other, non-represented boxes
  complete(port_name,box_id=full_seq(c(0,88),1),fill=list(prop_poly=0)) %>% 
  arrange(port_name,box_id) %>% 
  dplyr::select(port_name,box_id,prop_poly) %>% 
  pivot_wider(names_from="box_id",values_from = "prop_poly")

fp_atlantis_join %>% 
  filter(port_name=="Astoria") %>% 
  ggplot()+geom_sf(aes(fill=prop_poly))+
  labs(fill="Proportion of\nAtlantis Polygon")

# organize for final output
tribes_prop_polys<- tribes_atlantis_join %>%
  mutate(prop_poly=round(prop_poly,digits=6)) %>% 
  st_set_geometry(NULL) %>% 
  select(fleet,box_id,prop_poly) %>% 
  #fill in zeroes for other, non-represented boxes
  complete(fleet,box_id=full_seq(c(0,88),1),fill=list(prop_poly=0)) %>% 
  arrange(fleet,box_id) %>% 
  dplyr::select(fleet,box_id,prop_poly) %>% 
  pivot_wider(names_from="box_id",values_from = "prop_poly")

tribes_atlantis_join %>% 
  filter(fleet=="Tribal non-trawl") %>% 
  ggplot()+geom_sf(aes(fill=prop_poly))+
  labs(fill="Proportion of\nAtlantis Polygon")

options(scipen = 100)
# options(scipen = 0)
# write!
write.table(fp_prop_polys,here::here('data','atlantis','mpas_by_fleet_trawl.txt'),quote=F,row.names=F)
write.table(tribes_prop_polys,here::here('data','atlantis','mpas_by_fleet_tribes.txt'),quote=F,row.names=F)
