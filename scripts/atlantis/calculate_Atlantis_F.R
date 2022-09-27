# Calculate initial Atlantis F
# calculate F by fishery for Atlantis, using total catch data from WCGOP and collected biomass estimates

# total catch from WCGOP was organized in separate script "organize_Atlantis_total_catch.R"
library(tidyverse)


# fleets (with a manually-added set of columns to match to catch data)
fleets <- read_csv(here::here('data','atlantis','Atlantis_54fleets_definitions.csv'))

# catch data wcgop
catch <- read_csv(here::here('data','atlantis','catch_by_fleet_2013.csv'))

# biomass data
bio <- read_table(here::here('data','atlantis','outputCCV4_testBiomIndx.txt'))


# organize biomass data by Atlantis species code
bio <- bio %>% 
  # pull bottom trawl species codes only
  dplyr::select(Time,any_of(unique(catch$FG))) %>% 
  # take only the initial biomass, then convert to long form
  filter(Time==0) %>% 
  dplyr::select(-Time) %>% 
  pivot_longer(everything(),names_to="FG",values_to="biomass")

# make sure fleets align with Atlantis fleets (use matching key)
Atlantis_catch <- catch %>% 
  left_join(fleets,by=c("iopac","fishery_type")) %>% 
  filter(!is.na(Index)) %>% 
  group_by(Code,Index,Name,FG,fishery_type) %>% 
  summarise(totcatch=sum(totcatch)) %>% 
  ungroup()

# what catch are we missing?
# MISSING PORTS: PUGET SOUND, 'UNKNOWN PORTS', COASTAL WASHINGTON

catch_unaccounted <- catch %>% 
  left_join(fleets,by=c("iopac","fishery_type")) %>% 
  filter(is.na(Index)) %>% 
  group_by(iopac,Code,Index,Name,FG) %>% 
  summarise(totcatch=sum(totcatch)) %>% 
  ungroup()

# join and calculate F by fleet
Finit <- Atlantis_catch %>% 
  left_join(bio,by="FG") %>% 
  filter(!is.na(biomass)) %>% 
  # exploitation rate (proportion harvested in 2013)
  mutate(mu= totcatch/biomass) %>% 
  # convert to F (Atlantis User Guide Part II page 15)
  # F= -(1/t)ln(1-mu), but t=1
  mutate(F_yr=-log(1-mu)) %>% 
  # daily probability
  mutate(mFC=1-exp(-F_yr/365))

# make a block of text for pasting into Harvest.prm
harvest_block <- Finit %>% 
  dplyr::select(Index,FG,mFC) %>% 
  # fill in missing Index/code combos with zeroes
  complete(Index=full_seq(c(1,54),1),FG,fill=list(mFC=0)) %>% 
  mutate(prmName=paste('mFC',FG,sep="_")) %>% 
  arrange(prmName,Index) %>% 
  dplyr::select(prmName,Index,mFC) %>% 
  pivot_wider(names_from="Index",values_from = "mFC")

catch_block <- Finit %>% 
  dplyr::select(Index,FG,totcatch) %>% 
  # fill in missing Index/code combos with zeroes
  complete(Index=full_seq(c(1,54),1),FG,fill=list(totcatch=0)) %>% 
  mutate(prmName=paste('catch',FG,sep="_")) %>% 
  arrange(prmName,Index) %>% 
  dplyr::select(prmName,Index,totcatch) %>% 
  pivot_wider(names_from="Index",values_from = "totcatch")

options(scipen = 100)
# options(scipen = 0)
# write!
write.table(harvest_block,here::here('data','atlantis','mFC_by_fleet.txt'),quote=F,row.names=F)
write.table(catch_block,here::here('data','atlantis','catch_by_fleet.txt'),quote=F,row.names=F )
write_csv(Finit,here::here('data','atlantis','Fcalc_by_fleet.csv'))


#### Visualize? ####
spp_match <- read_csv(here('data','atlantis','sp_match_catch_FG_Owen.csv'))
fg_match <- read_csv(here('data','atlantis','atlantis_fg_match.csv')) %>% 
  rename(FG=`Atlantis code`,grp_name=`Group name`)

FDC_catchplot <- Atlantis_catch %>% 
  left_join(fg_match) %>% 
  filter(FG=="FDC",totcatch>20) %>% 
  ggplot(aes(fct_reorder(Name,totcatch,.desc = T),totcatch))+
  geom_col()+
  labs(x="Atlantis Fleet",y="Deep Small Rockfish Catch")+
  theme(axis.text.x=element_text(angle=45,vjust=0.9,hjust=0.8))

FDC_Fplot <- Finit %>% 
  left_join(fg_match) %>% 
  filter(FG=="FDC",totcatch>20) %>% 
  ggplot(aes(fct_reorder(Name,mFC,.desc = T),mFC))+
  geom_col()+
  labs(x="Atlantis Fleet",y="Deep Small Rockfish Daily F")+
  theme(axis.text.x=element_text(angle=45,vjust=0.9,hjust=0.8))

FMN_catchplot <- Atlantis_catch %>% 
  left_join(fg_match) %>% 
  filter(FG=="FMN",totcatch>100) %>% 
  ggplot(aes(fct_reorder(Name,totcatch,.desc = T),totcatch))+
  geom_col()+
  labs(x="Atlantis Fleet",y="Sablefish Catch")+
  theme(axis.text.x=element_text(angle=45,vjust=0.9,hjust=0.8))
FMN_Fplot <- Finit %>% 
  left_join(fg_match) %>% 
  filter(FG=="FMN",totcatch>100) %>% 
  ggplot(aes(fct_reorder(Name,mFC,.desc = T),mFC))+
  geom_col()+
  labs(x="Atlantis Fleet",y="Sablefish Daily F")+
  theme(axis.text.x=element_text(angle=45,vjust=0.9,hjust=0.8))

DUN_catchplot <- Atlantis_catch %>% 
  left_join(fg_match) %>% 
  filter(FG=="DUN",totcatch>100) %>% 
  ggplot(aes(fct_reorder(Name,totcatch,.desc = T),totcatch))+
  geom_col()+
  labs(x="Atlantis Fleet",y="Dungeness Catch")+
  theme(axis.text.x=element_text(angle=45,vjust=0.9,hjust=0.8))

DUN_Fplot <- Finit %>% 
  left_join(fg_match) %>% 
  filter(FG=="DUN",totcatch>100) %>% 
  ggplot(aes(fct_reorder(Name,mFC,.desc = T),mFC))+
  geom_col()+
  labs(x="Atlantis Fleet",y="Dungeness Daily F")+
  theme(axis.text.x=element_text(angle=45,vjust=0.9,hjust=0.8))

FDC_catchplot
FDC_Fplot
FMN_catchplot
FMN_Fplot
DUN_catchplot
DUN_Fplot

# which FGs appear in all fishery types?
x<- Atlantis_catch %>% 
  left_join(fg_match) %>% 
  group_by(FG,fishery_type) %>% 
  summarise(tot=sum(totcatch)) %>% 
  ungroup() %>% 
  group_by(FG) %>% 
  filter(tot>0) %>% 
  ungroup()
FMN_pie <- Atlantis_catch %>% 
  left_join(fg_match) %>% 
  filter(FG=="FMN") %>% 
  group_by(FG,fishery_type) %>% 
  summarise(tot=sum(totcatch)) %>% 
  ungroup() %>% 
  ggplot(aes(x="",y=tot,fill=fishery_type))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Fishery\nType",title="Sablefish")
FMN_pie

DOG_pie <- Atlantis_catch %>% 
  left_join(fg_match) %>% 
  filter(FG=="DOG") %>% 
  group_by(FG,fishery_type) %>% 
  summarise(tot=sum(totcatch)) %>% 
  ungroup() %>% 
  ggplot(aes(x="",y=tot,fill=fishery_type))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(fill="Fishery\nType",title="Spiny Dogfish")
DOG_pie

