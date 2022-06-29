## Plot sample size for wc trawl survey species
## using the same set of data as sdmTMB models
## WC groundfish trawl survey 2003-2018
# note: there are already some rare species removed from these lists (see pull_wc_trawl_data.R)

library(tidyverse)
library(here)
library(forcats)

dat<- read_rds(here('data','nwfsc_trawl_data.rds'))

# number of unique species
length(unique(dat$species))
# 81

# species list, organized by number of positive occurrences (i.e., CPUE > 0)
species_list_occurrence <- dat %>% 
  mutate(occur=cpue_kg_km2>0) %>% 
  group_by(species) %>% 
  summarise(num_pos=sum(occur)) %>% 
  arrange(desc(num_pos)) %>% 
  mutate(species_ordered=fct_reorder(species,desc(num_pos)))

# a little plot
species_list_occurrence %>% 
  ggplot(aes(species_ordered,num_pos))+
  geom_col()+
  theme_minimal()+
  labs(x="Species",y="Number of Positive Occurrences, 2003-2018")+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,))

# similar, but for mean positive density (cpue/km2,excluding zeroes)
species_list_density <- dat %>% 
  filter(cpue_kg_km2>0) %>% 
  group_by(species) %>% 
  summarise(mean_dens=mean(cpue_kg_km2)) %>% 
  arrange(desc(mean_dens)) %>% 
  mutate(species_ordered=fct_reorder(species,desc(mean_dens)))

species_list_density %>% 
  ggplot(aes(species_ordered,mean_dens))+
  geom_col()+
  theme_minimal()+
  labs(x="Species",y="Mean Positive Density (kg/km2), 2003-2018")+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,))
