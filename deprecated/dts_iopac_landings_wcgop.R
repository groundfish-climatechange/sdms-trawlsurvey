# Check DTS landings by IOPAC in the WCGOP data
library(tidyverse)
library(here)
library(ggsci)
options(dplyr.summarise.inform=FALSE)
# legend
meta <- read_csv(here('data','wcgop','legend.csv'))

# load the data
# ifq catch shares species
dat_ifq <- read_csv(here('data','wcgop','ifq_species_cs_sectors_2011_2020.csv'))
dtsspp <- c("Shortspine Thornyhead","Sablefish","Dover Sole","Longspine Thornyhead")

dts <- dat_ifq %>% 
  filter(species %in% dtsspp) %>% 
  group_by(iopac,species) %>% 
  summarise(totland=sum(ret_mt)) %>% 
  ungroup() %>% 
  group_by(iopac) %>% 
  mutate(allspp=sum(totland)) %>% 
  ungroup() %>% 
  filter(allspp>0)

ggplot(dts,aes(fct_reorder(iopac,desc(allspp)),totland/1000,fill=species))+
  geom_col()+
  scale_fill_aaas()+
  labs(y="Total Retained 2011-2020 (1000s MT)",fill="Species",x="IOPAC")+
  theme(axis.text.x=element_text(angle=90,vjust=0))
