###############################################
###############################################
######### Reshaping SDMs to Atlantis input files
###############################################
###############################################
# 01.04.22
# Modifying from Pierre-Yves Hernvann and Barb Muhling's code
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(rbgm) # Mike Sumner's Atlantis package for dealing with BGM files
library(ncdf4)
library(tidync)
library(tictoc)
library(RNetCDF)

#######
## Loading polygons (Owen, you may have the polygon file in your toolbox)

bgm <- read_bgm(here("data",'grids',"CalCurrentV3_utm.bgm"))
emocc_sf <- box_sf(bgm) %>% dplyr::select(box_id,label,area) %>% 
  st_transform("+proj=utm +zone=10 +datum=WGS84 +units=km")


# Look at an example ncdf
# copy to new
# file.copy(here('data','JAC_distrib2.nc'),to=here('data','tempnc.nc'),overwrite = T)
# flx <- here('data','tempnc.nc')
# tidync(flx)
# x <- open.nc(flx)
# close.nc(x)
# 
# times <- tidync(flx) %>% activate("D0") %>% hyper_tibble() %>% 
#   mutate(date=as_date("2013-01-01 00:00:00")+seconds(t))

#######
## Defining the key variables
# defining the time-range for which we will formulate the distributions
# use lubridate for precise dates and times
time_seq <- crossing(year=2013:2099,month=1:12,day="01") %>% 
  unite(date,year,month,day,sep="-") %>% 
  mutate(date=ymd(date)) %>% 
  mutate(seconds=date[1]%--%date %>% seconds() %>% as.numeric())

# sppdf <- time_seq %>% 
#   mutate(box_id=0) %>% 
#   complete(nesting(date,seconds),box_id=unique(emocc_sf$box_id)) %>% 
#   mutate(year=year(date)) %>% 
#   left_join(rel_est,by = c("year", "box_id"))
# data_array <- array(sppdf$rel_est,dim=c(length(unique(sppdf$box_id)),length(unique(sppdf$seconds))))

make_nc <- function(atlantis_rds,sppname,nc_name,times=time_seq,this_title,nbox=89, stage="juvenile",
                    this_geom="CalCurrentV3_utm.bgm",t_units='seconds since 2013-01-01 00:00:00'){
  
  # make data array from sdm .rds
  # estimates of relative distribution by atlantis box
  rel_est <- atlantis_rds %>% 
    dplyr::select(box_id,year,rel_est)
  
  sppdf <- time_seq %>% 
    mutate(box_id=0) %>% 
    complete(nesting(date,seconds),box_id=unique(emocc_sf$box_id)) %>% 
    mutate(year=year(date)) %>% 
    left_join(rel_est,by = c("year", "box_id"))
  data_array <- array(sppdf$rel_est,dim=c(length(unique(sppdf$box_id)),length(unique(sppdf$seconds))))
  
  # construct the nc file
  nc_file <- create.nc(nc_name)
  
  if(stage=="juvenile") {varname <- paste0(sppname,"_stage_0")} else{varname<-paste0(sppname,"_stage_1")}
  
  dim.def.nc(nc_file, "t", unlim=TRUE)
  dim.def.nc(nc_file, "b", nbox) # manual
  
  var.def.nc(nc_file, "t", "NC_DOUBLE", "t")
  var.def.nc(nc_file,"t1","NC_INT","t")
  var.def.nc(nc_file, varname, "NC_DOUBLE", c("b","t"))
  
  att.put.nc(nc_file, varname, "_FillValue", "NC_DOUBLE", 0)
  att.put.nc(nc_file, "t", "units", "NC_CHAR", t_units)
  att.put.nc(nc_file, "t", "dt", "NC_DOUBLE", 86400)
  att.put.nc(nc_file,"t1","long_name","NC_CHAR","t1")
  att.put.nc(nc_file, "NC_GLOBAL", "title", "NC_CHAR", this_title)
  att.put.nc(nc_file, "NC_GLOBAL", "geometry", "NC_CHAR", this_geom)
  att.put.nc(nc_file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc_file, "t", time_seq$seconds)
  var.put.nc(nc_file,"t1",1:nrow(time_seq))
  var.put.nc(nc_file, varname, data_array)
  close.nc(nc_file)
}

# for calibrating Atlantis, we want looped SDMs (i.e., looping 2013 but with a timeseries)
make_looped_nc <- function(atlantis_rds,sppname,nc_name,times=time_seq,this_title,nbox=89, stage="juvenile",
                    this_geom="CalCurrentV3_utm.bgm",t_units='seconds since 2013-01-01 00:00:00'){
  
  # make data array from sdm .rds
  # estimates of relative distribution by atlantis box
  rel_est <- atlantis_rds %>% 
    dplyr::select(box_id,year,rel_est) %>% 
    filter(year==2013)
  
  # this is where the difference in from the other function: manually copy year 2013
  loopyrs <- purrr::map_df(2014:2100,function(yr)rel_est %>% mutate(year=yr))
  # attach to the 2013 year
  rel_est <- rel_est %>% bind_rows(loopyrs)
  
  sppdf <- time_seq %>% 
    mutate(box_id=0) %>% 
    complete(nesting(date,seconds),box_id=unique(emocc_sf$box_id)) %>% 
    mutate(year=year(date)) %>% 
    left_join(rel_est,by = c("year", "box_id"))
  data_array <- array(sppdf$rel_est,dim=c(length(unique(sppdf$box_id)),length(unique(sppdf$seconds))))
  
  # construct the nc file
  nc_file <- create.nc(nc_name)
  
  if(stage=="juvenile") {varname <- paste0(sppname,"_stage_0")} else{varname<-paste0(sppname,"_stage_1")}
  
  dim.def.nc(nc_file, "t", unlim=TRUE)
  dim.def.nc(nc_file, "b", nbox) # manual
  
  var.def.nc(nc_file, "t", "NC_DOUBLE", "t")
  var.def.nc(nc_file,"t1","NC_INT","t")
  var.def.nc(nc_file, varname, "NC_DOUBLE", c("b","t"))
  
  att.put.nc(nc_file, varname, "_FillValue", "NC_DOUBLE", 0)
  att.put.nc(nc_file, "t", "units", "NC_CHAR", t_units)
  att.put.nc(nc_file, "t", "dt", "NC_DOUBLE", 86400)
  att.put.nc(nc_file,"t1","long_name","NC_CHAR","t1")
  att.put.nc(nc_file, "NC_GLOBAL", "title", "NC_CHAR", this_title)
  att.put.nc(nc_file, "NC_GLOBAL", "geometry", "NC_CHAR", this_geom)
  att.put.nc(nc_file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc_file, "t", time_seq$seconds)
  var.put.nc(nc_file,"t1",1:nrow(time_seq))
  var.put.nc(nc_file, varname, data_array)
  close.nc(nc_file)
}

# Test
# dover_rds <- read_rds(here('model output','atlantis sdms','dover sole.rds'))
# make_nc(atlantis_rds=dover_rds,sppname="FDP",nc_name =here('model output','atlantis sdms', "FDP.nc"),this_title="Dover sole spatial dists")
# x<-nc_open(here('model output','atlantis sdms', "FDP.nc"))
# nc_close(x)

dover_rds <- read_rds(here('model output','atlantis sdms','FDP.rds'))
make_looped_nc(atlantis_rds=dover_rds,sppname="FDP",nc_name =here('model output','atlantis sdms', "FDP.nc"),this_title="Dover sole spatial dists")
x<-(nc_open(here('model output','atlantis sdms', "FDP.nc")))
nc_close(x)
# 
# y <- nc_open(here('data','JAC_distrib2.nc'))
# y

# apply to all models
fls <- list.files(here('model output','atlantis sdms')) %>% str_subset(".rds") %>% str_subset("_",negate=T) %>% str_remove(".rds")

# the files need a specific naming convention, namely CODE_distrib_0 or CODE_distrib_1, where CODE is the species code, 
# and 0 or 1 denotes juveniles and adults, respectively. For now, adult and juvenile distributions are identical for groundfish

# make juveniles
purrr::map(fls,function(fl){
  tic(paste("Making ncdf for",fl))
  x <- read_rds(here('model output','atlantis sdms',paste0(fl,'.rds')))
  make_nc(atlantis_rds=x,sppname=fl,nc_name=here('model output','atlantis sdms',paste0(fl,'_stage_0.nc')),
          stage = "juvenile",
          this_title=paste(fl,"spatial dists"))
  toc()
})

# make adults
purrr::map(fls,function(fl){
  tic(paste("Making ncdf for",fl))
  x <- read_rds(here('model output','atlantis sdms',paste0(fl,'.rds')))
  make_nc(atlantis_rds=x,sppname=fl,nc_name=here('model output','atlantis sdms',paste0(fl,'_stage_1.nc')),
          stage = "adult",
          this_title=paste(fl,"spatial dists"))
  toc()
})

## FOR LOOPED 2013
# make juveniles
purrr::map(fls,function(fl){
  tic(paste("Making ncdf for",fl))
  x <- read_rds(here('model output','atlantis sdms',paste0(fl,'.rds')))
  make_looped_nc(atlantis_rds=x,sppname=fl,nc_name=here('model output','atlantis sdms','looped 2013',paste0(fl,'_stage_0.nc')),
          stage = "juvenile",
          this_title=paste(fl,"spatial dists"))
  toc()
})

# make adults
purrr::map(fls,function(fl){
  tic(paste("Making ncdf for",fl))
  x <- read_rds(here('model output','atlantis sdms',paste0(fl,'.rds')))
  make_looped_nc(atlantis_rds=x,sppname=fl,nc_name=here('model output','atlantis sdms','looped 2013',paste0(fl,'_stage_1.nc')),
          stage = "adult",
          this_title=paste(fl,"spatial dists"))
  toc()
})
