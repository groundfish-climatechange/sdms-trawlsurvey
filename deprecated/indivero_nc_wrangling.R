# working with GLORYS .nc files
library(tidyverse)
library(tidync)
library(sf)
library(viridis)

fn <- "C:/Users/Owen.Liu/Downloads/cmems_mod_glo_bgc_my_0.25_P1D-m_1691617771255.nc"

nc <- tidync(fn)
nc

# get a sense of the dimensions
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

# try the method with just one time slice
# first time step of data
nc1 <- nc %>% hyper_filter(time=index==1) %>% hyper_tibble()
# filter for bottom-most depth layer
nc1_bottom <- nc1 %>% 
  group_by(longitude,latitude) %>% 
  filter(depth==max(depth))
# make sure this worked and there's only 1 observation per location
nc1_check <- nc1_bottom %>% count(longitude,latitude)
unique(nc1_check$n) # yep, only 1 obs per loc


# plot to check it out
nc1_bottom %>% 
  st_as_sf(coords=c("longitude","latitude"),remove = F,crs=4326) %>% 
  ggplot(aes(color=o2))+
  geom_sf()+
  scale_color_viridis()
# looks about right

# apply to all time periods and vectorize
# function to do the above for one time step
extract_bottom_vars <- function(tstep){
  nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
    hyper_tibble() %>% 
  # filter for bottom-most depth layer
    group_by(longitude,latitude) %>% 
    filter(depth==max(depth))
  nc_df
}

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

# if you want time in intelligible units (instead of 'hours since 1950')
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

