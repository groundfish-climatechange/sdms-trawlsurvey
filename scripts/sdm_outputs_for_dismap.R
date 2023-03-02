# Four DTS species for John Kennedy/DisMAP
# January 2023
# Owen R. Liu
library(tidyverse)

# ROMS

roms <- read_rds(here('data','roms_for_sdm_projection.rds'))

# combined across ESMs
dover_ens_all <- read_rds(here('model output','dts paper','dover_ensemble_preds_combined.rds')) %>% 
  mutate(species='dover')
sable_ens_all <- read_rds(here('model output','dts paper','sable_ensemble_preds_combined.rds'))%>% 
  mutate(species='sable')
ss_ens_all <- read_rds(here('model output','dts paper','ss_ensemble_preds_combined.rds'))%>% 
  mutate(species='ss')
ls_ens_all <- read_rds(here('model output','dts paper','ls_ensemble_preds_combined.rds'))%>% 
  mutate(species='ls')

four_spp <- list(dover_ens_all,sable_ens_all,ss_ens_all,ls_ens_all) %>% bind_rows() %>% 
  # join roms data
  left_join(roms,by = c("year", "longitude", "latitude", "lat", "lon"))

four_spp2 <- four_spp %>% dplyr::select(year, lon_UTM=longitude,lat_UTM=latitude, lat, lon, depth_m, species, median_est, mean_est, est5, est95) %>% 
  mutate(spp_sci=case_when(
    species=="dover" ~ "Microstomus pacificus",
    species=="sable" ~ "Anoplopoma fimbria",
    species=="ss" ~ "Sebastolobus alascanus",
    species=="ls" ~ "Sebastolobus altivelis"
  )) %>% 
  mutate(spp_common=case_when(
    species=="dover" ~ "Dover sole",
    species=="sable" ~ "Sablefish",
    species=="ss" ~ "Shortspine thornyhead",
    species=="ls" ~ "Longspine thornyhead"
  )) %>% 
  dplyr::select(-species)
  
glimpse(four_spp2)


projection_extent <- read_rds(here::here('data','cropped_domain_for_projected_sdms.rds'))

x <- four_spp2 %>% distinct(lon_UTM,lat_UTM)
