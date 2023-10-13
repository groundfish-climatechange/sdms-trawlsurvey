# Organize lingcod and petrale sole model outputs for Nerea
# 08/30/2023

library(tidyverse)
library(viridis)
library(here)

# lingcod projections, 3 ESMs
ling <- read_rds(here('model output','lingcod','projection_3ESMs.rds'))
glimpse(ling)

#petrale projections, 3 ESMs
pet <- read_rds(here('model output','petrale sole','projection_3ESMs.rds'))
glimpse(pet)

# quick map

pet %>% filter(year==1980,esm=='hadl') %>% 
  ggplot(aes(lon,lat,color=est))+
  geom_point()

ling %>% filter(year==2100,esm=='hadl') %>% 
  ggplot(aes(lon,lat,color=est))+
  geom_point()

# Calculate example output for petrale sole
pet_historical <- pet %>% 
  filter(year %in% c(1985:2015)) %>% 
  group_by(esm,lat,lon,latitude,longitude) %>% 
  summarise(est=mean(est,na.rm=T)) %>% 
  mutate(cpue_hist=exp(est)) %>%
  dplyr::select(-est) %>%  
  ungroup()
pet_mid <- pet %>% 
  filter(year %in% c(2035:2055)) %>% 
  group_by(esm,lat,lon,latitude,longitude) %>% 
  summarise(est=mean(est,na.rm=T)) %>% 
  mutate(cpue_mid=exp(est))%>%
  dplyr::select(-est) %>%  
  ungroup()
pet_end <- pet %>% 
  filter(year %in% c(2075:2100)) %>% 
  group_by(esm,lat,lon,latitude,longitude) %>% 
  summarise(est=mean(est,na.rm=T)) %>% 
  mutate(cpue_end=exp(est))%>%
  dplyr::select(-est) %>%  
  ungroup()

pet_all <- pet_historical %>% 
  left_join(pet_mid,by=c("esm","lat","lon","latitude","longitude")) %>% 
  left_join(pet_end,by=c("esm","lat","lon","latitude","longitude"))

# look at distribution of values
pet_all %>%
  pivot_longer(contains("cpue"),names_to = "period",values_to="cpue") %>% 
  filter(cpue>1) %>% 
  ggplot(aes(cpue,color=period))+
  geom_density()+
  facet_wrap(~esm)

# HISTORICAL MAP
pet_historical %>% 
  ggplot(aes(longitude,latitude,color=cpue_hist))+
  geom_point()+
  facet_wrap(~esm)+
  scale_color_viridis()
# DIFFERENCE MAP
pet_diff <- pet_all %>% 
  mutate(mid_diff=cpue_mid-cpue_hist,
         end_diff=cpue_end-cpue_hist)

# middle of century habitat change relative to historical
pet_diff %>% 
  ggplot(aes(longitude,latitude,color=mid_diff))+
  geom_point()+
  scale_color_gradient2()

# end of century habitat change relative to historical
pet_diff %>% 
  ggplot(aes(longitude,latitude,color=end_diff))+
  geom_point()+
  scale_color_gradient2()


# Calculate example output for lingcod
ling_historical <- ling %>% 
  filter(year %in% c(1985:2015)) %>% 
  group_by(year,lat,lon,latitude,longitude) %>% 
  summarise(est=mean(est,na.rm=T)) %>% 
  mutate(cpue_hist=exp(est)) %>%
  dplyr::select(-est) %>%  
  ungroup()
ling_mid <- ling %>% 
  filter(year %in% c(2035:2055)) %>% 
  group_by(esm,lat,lon,latitude,longitude) %>% 
  summarise(est=mean(est,na.rm=T)) %>% 
  mutate(cpue_mid=exp(est))%>%
  dplyr::select(-est) %>%  
  ungroup()
ling_end <- ling %>% 
  filter(year %in% c(2075:2100)) %>% 
  group_by(esm,lat,lon,latitude,longitude) %>% 
  summarise(est=mean(est,na.rm=T)) %>% 
  mutate(cpue_end=exp(est))%>%
  dplyr::select(-est) %>%  
  ungroup()

ling_all <- ling_historical %>% 
  left_join(ling_mid,by=c("esm","lat","lon","latitude","longitude")) %>% 
  left_join(ling_end,by=c("esm","lat","lon","latitude","longitude"))

# look at distribution of values
ling_all %>%
  pivot_longer(contains("cpue"),names_to = "period",values_to="cpue") %>% 
  filter(cpue>1) %>% 
  ggplot(aes(cpue,color=period))+
  geom_density()+
  facet_wrap(~esm)

# HISTORICAL MAP
ling_historical %>% 
  ggplot(aes(longitude,latitude,color=cpue_hist))+
  geom_point()+
  facet_wrap(~esm)+
  scale_color_viridis()
# DIFFERENCE MAP
ling_diff <- ling_all %>% 
  mutate(mid_diff=cpue_mid-cpue_hist,
         end_diff=cpue_end-cpue_hist)

# middle of century habitat change relative to historical
ling_diff %>% 
  ggplot(aes(longitude,latitude,color=mid_diff))+
  geom_point()+
  scale_color_gradient2()

# end of century habitat change relative to historical
ling_diff %>% 
  ggplot(aes(longitude,latitude,color=end_diff))+
  geom_point()+
  scale_color_gradient2()
