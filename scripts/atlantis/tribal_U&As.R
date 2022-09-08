# Manually create Tribal U&As
# from here: http://resource-analysis.com/wp-content/uploads/2018/10/Usual-and-Accustomed-Fishing-Areas_20121022.pdf

library(tidyverse)
library(sf)
library(rbgm)
library(terra)
library(fasterize)

sf_use_s2(T)

# US EEZ
eez <- read_sf(here::here('data','us_westcoast_eez.shp'))
bbox_eez <- st_bbox(eez)

# quick function to convert degress/minutes/seconds to decimal degrees
degms_to_dd <- function(deg,m,s){
  deg+(m*60+s)/3600
}

#### MANUALLY CREATE TRIBAL U&A POLYGONS ####
# Makah—That portion of the FMA north of 48°02′15″ N. lat. (Norwegian Memorial) and east of 125°44′00″ W. long
# In the Strait of Juan de Fuca: 123° 42' 10.34"
makahcrop <- c(xmin=-degms_to_dd(125,44,0),ymin=degms_to_dd(48,2,15),xmax=-degms_to_dd(123,42,10.34),ymax=bbox_eez['ymax'])
makahbox <- list(matrix(makahcrop[c(1,2,1,4,3,4,3,2,1,2)],ncol = 2,byrow = T)) %>% st_polygon() %>% 
  st_sfc(crs=4326)
makah <- eez %>% st_intersection(makahbox)

# Quileute—That portion of the FMA between 48°07′36″ N. lat. (Sand Point) and 47°31′42″ N. lat.(Queets River) and east of 125°44′00″ W. long
quilcrop <- c(xmin=-degms_to_dd(125,44,0),ymin=degms_to_dd(47,31,42),xmax=-124.25,ymax=degms_to_dd(48,07,36))
quilbox <- list(matrix(quilcrop[c(1,2,1,4,3,4,3,2,1,2)],ncol = 2,byrow = T)) %>% st_polygon() %>% 
  st_sfc(crs=4326)
quil <- eez %>% st_intersection(quilbox)

#Hoh—That portion of the FMA between 47°54′18″ N. lat. (Quillayute River) and 47°21′00″ N. lat.(Quinault River) and east of 125°44′00″ W. long
hohcrop <- c(xmin=-degms_to_dd(125,44,0),ymin=degms_to_dd(47,21,00),xmax=-124.25,ymax=degms_to_dd(47,54,18))
hohbox <- list(matrix(hohcrop[c(1,2,1,4,3,4,3,2,1,2)],ncol = 2,byrow = T)) %>% st_polygon() %>% 
  st_sfc(crs=4326)
hoh <- eez %>% st_intersection(hohbox)

#Quinault—That portion of the FMA between 47°40′06″ N. lat. (Destruction Island) and 46°53′18″ N.lat. (Point Chehalis) and east of 125°44′00″ W. long.
quincrop <- c(xmin=-degms_to_dd(125,44,0),ymin=degms_to_dd(46,53,18),xmax=-degms_to_dd(123,42,10.34),ymax=degms_to_dd(47,40,06))
quinbox <- list(matrix(quincrop[c(1,2,1,4,3,4,3,2,1,2)],ncol = 2,byrow = T)) %>% st_polygon() %>% 
  st_sfc(crs=4326)
quin <- eez %>% st_intersection(quinbox)

# All
tribalUAs <- bind_rows(makah,quil,hoh,quin) %>% 
  mutate(UA=c("Makah","Quileute","Hoh","Quinault")) %>% 
  dplyr::select(UA)

#### CROP WITH 700 FATHOM LINE ####
# hi-res bathymetry
bathy <- terra::rast("C:/Users/Owen.Liu/Documents/github/fish-footprints/GIS_Data/crm_bathy.tiff") %>% 
  # crop to extent of tribal UAs
  crop(ext(tribalbbox[c(1,3,2,4)]))

# reclassify to cut out areas deeper than the 700fathom depth contour
rcl <- c(-Inf,-700*1.8288-1,NA,
         -700*1.8288-1,Inf,1) %>% matrix(ncol=3,byrow=T)
bathy_rcl = classify(bathy,rcl)

# create a mask for the tribal UAs
tribalUAs_bathy_mask <- terra::mask(bathy_rcl,vect(tribalUAs)) %>% as.polygons() %>% project("epsg:32610")
# 
plot(tribalUAs_bathy_mask)

# now, create a final version of the tribal UAs that cut out areas deeper than 700fms
tribalUAs_utm <- tribalUAs %>% st_transform(32610)
masksf <- st_as_sf(tribalUAs_bathy_mask) %>% st_make_valid()
tribalUAs_with_700fm_cutoff <- tribalUAs_utm %>% st_intersection(masksf) %>% 
  # convert back to latlon
  st_transform(4326)


#### Plot ####

ggplot(tribalUAs)+geom_sf(aes(fill=UA),alpha=0.5) #original
ggplot(tribalUAs_with_700fm_cutoff)+geom_sf(aes(fill=UA),alpha=0.5)#700fathom cutoff

# with Atlantis in the background
tribalbbox <- st_bbox(tribalUAs)
bgm <- read_bgm(here('data','CalCurrentV3_utm.bgm'))
atlantis_sf <- box_sf(bgm) %>% select(label,.bx0,box_id,poly_area=area)
st_crs(atlantis_sf) <- st_crs(attr(atlantis_sf$geometry, "crs")$proj)
atlantis_sf <- atlantis_sf %>% st_transform(4326)
ggplot()+geom_sf(data=atlantis_sf,fill=NA,col='black')+geom_sf(data=tribalUAs,aes(fill=UA),alpha=0.5)+
  xlim(tribalbbox[1],tribalbbox[3])+ylim(tribalbbox[2],tribalbbox[4])


#### WRITE ####
st_write(tribalUAs,here::here('data','tribal_U&As.shp'))
st_write(tribalUAs_with_700fm_cutoff,here::here('data','tribal_U&As_with_700fm_cutoff.shp'))
