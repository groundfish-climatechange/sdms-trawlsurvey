# create example rasters for John Kennedy and DisMAP
library(tidyverse)
library(sf)
library(here)
library(terra)
library(RANN)
library(stars)

# projection extent
projection_extent <- read_rds(here::here('data','cropped_domain_for_projected_sdms.rds'))

# make grid for raster maps
gr <- projection_extent %>% 
  st_make_grid(cellsize=10,what='centers') %>% 
  st_as_sf() %>% 
  st_intersection(projection_extent)
gr_xy <- st_coordinates(gr)

# example file for testing
longnose_file<-here::here('Janelle','models_owen','longnose skate','projection_3ESMs.rds')
x <- read_rds(longnose_file) %>% filter(esm=="gfdl")
yr_vec <- 2000

create_annual_raster <- function(preddf,yr_vec=c(2000)){
  df <- preddf %>% 
    filter(year%in%yr_vec) %>% 
    group_by(longitude,latitude) %>% 
    summarise(est=mean(est,na.rm=T) %>% exp()) %>% 
    ungroup()
  # match nearest neighbors from predictions to grid
  pred_points <- df %>% dplyr::select(longitude,latitude) %>% as.matrix()
  nns <- nn2(pred_points,gr_xy,k=1)$nn.idx
  gr_pred <- gr_xy %>% 
    as_tibble() %>% 
    mutate(wtcpue=df$est[nns] %>% round(25)) %>% 
    rast(crs='+proj=utm +zone=10 +datum=WGS84 +units=km',type='xyz')
  gr_pred
}

# make a raster stack and save
rs <- map(1980:2100,create_annual_raster,preddf=x) %>% do.call(c,.)
names(rs) <- as.character(1980:2100)

# write
writeRaster(rs,here('Janelle','models_owen','longnose skate gfdl 1980_2100.tif'),filetype="GTiff")
