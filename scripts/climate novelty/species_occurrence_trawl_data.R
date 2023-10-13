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
# 91

# species list, organized by number of positive occurrences (i.e., CPUE > 0)
species_list_occurrence <- dat %>% 
  mutate(occur=cpue_kg_km2>0) %>% 
  group_by(species,scientific_name) %>% 
  summarise(num_pos=sum(occur)) %>% 
  ungroup() %>% 
  # find percentile rank relative to other species in the survey
  mutate(occur_rank=percent_rank(num_pos)*100)%>% 
  dplyr::select(species,scientific_name,num_pos,occur_rank)

# a little plot
species_list_occurrence %>% 
  mutate(species_ordered=fct_reorder(species,desc(num_pos))) %>% 
  filter(occur_rank>=10) %>% 
  ggplot(aes(species_ordered,num_pos))+
  geom_col()+
  theme_minimal()+
  labs(x="Species",y="Number of Positive Occurrences, 2003-2018")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))

# similar, but for mean positive density (cpue/km2,excluding zeroes)
species_list_density <- dat %>% 
  filter(cpue_kg_km2>0) %>% 
  group_by(species,scientific_name) %>% 
  summarise(mean_dens=mean(cpue_kg_km2)) %>% 
  ungroup() %>% 
  arrange(desc(mean_dens)) %>% 
  # find percentile rank relative to other species in the survey
  mutate(density_rank=percent_rank(mean_dens)*100) %>% 
  dplyr::select(species,scientific_name,mean_dens,density_rank)

species_list_density %>% 
  mutate(species_ordered=fct_reorder(species,desc(mean_dens))) %>% 
  filter(density_rank>=10) %>% 
  ggplot(aes(species_ordered,mean_dens))+
  geom_col()+
  theme_minimal()+
  labs(x="Species",y="Mean Positive Density (kg/km2), 2003-2018")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))

# compare to CVA species list
spplist <- read_csv(here('data','species_CVA.csv')) %>% 
  mutate(sci=tolower(Scientific),
         spp=tolower(Species))

cva_occurrence_density <- spplist %>% 
  left_join(species_list_occurrence,by=c('sci'='scientific_name')) %>% 
  left_join(species_list_density,by=c('sci'='scientific_name','species')) %>% 
  dplyr::select(Species,Scientific,num_pos:density_rank)
write_csv(cva_occurrence_density,here('data','cva_trawl_survey_list.csv'))

