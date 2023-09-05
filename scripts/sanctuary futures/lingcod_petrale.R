# Organize lingcod and petrale sole model outputs for Nerea
# 08/30/2023

library(tidyverse)
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
