# Data from Kate Richerson 07/10/2022
library(tidyverse)
library(here)
options(dplyr.summarise.inform=FALSE)
# legend
meta <- read_csv(here('data','wcgop','legend.csv'))

# load the data
# ifq catch shares species
dat_ifq <- read_csv(here('data','wcgop','ifq_species_cs_sectors_2011_2020.csv'))
# non catch-share sectors
dat_allspp_noncs <- read_csv(here('data','wcgop','all_species_non_cs_sectors_2011_2021.csv'))
# non-ifq species in catch share sectors
dat_nonifq <- read_csv(here('data','wcgop','nonifq_species_cs_sectors_2011_2020.csv'))
# all species catch share sectors 2021
dat_allspp_cs_2021 <- read_csv(here('data','wcgop','all_species_cs_sectors_2021.csv'))


# find unique species to match to atlantis groups
spp1 <- dat_ifq %>% distinct(species,scientific_name)
spp2 <- dat_allspp_noncs %>% distinct(species,scientific_name)
spp3 <- dat_nonifq %>% distinct(species,scientific_name)
spp4 <- dat_allspp_cs_2021 %>% distinct(species,scientific_name)

spp_needed <- bind_rows(spp1,spp2,spp3,spp4) %>% distinct() %>% arrange(species)
# write_csv(spp_needed,here('data','wcgop','spp_match_needs.csv'))

# Matching key
spp_match <- read_csv(here('data','atlantis','sp_match_catch_FG_Owen.csv'))

# summarize
dat_ifq_atl <- dat_ifq %>% 
  left_join(spp_match,by=c('species','scientific_name')) %>% 
  mutate(ret_dt_mt=ret_mt+dis_mt,
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,species,FG,Keep_Atlantis) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

# now for the other sectors
### Non-catch share sectors ###
# this one has multiple sectors and gear--- sum across all?
dat_allspp_noncs_atl <- dat_allspp_noncs %>% 
  filter(sector!="Tribal") %>% # remove tribal catch
  left_join(spp_match,by=c('species','scientific_name')) %>% 
  # fill in zeroes and replace infinites
  mutate(ret_mt=replace_na(ret_mt,0),
         est_dis_mt=replace_na(est_dis_mt,0),
         est_dis_mt=ifelse(is.infinite(est_dis_mt),0,est_dis_mt)) %>% 
  mutate(ret_dt_mt=ret_mt+est_dis_mt, # estimate of discards using ratios
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,species,FG,Keep_Atlantis) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

## Tribal fisheries
dat_tribal_atl <- dat_allspp_noncs %>% 
  filter(sector=="Tribal") %>% 
  left_join(spp_match,by=c('species','scientific_name')) %>%
  # fill in zeroes
  mutate(ret_mt=replace_na(ret_mt,0),
         est_dis_mt=replace_na(est_dis_mt,0)) %>% 
  mutate(ret_dt_mt=ret_mt+est_dis_mt, # estimate of discards using ratios
         year=as.integer(year))%>% 
  group_by(iopac,year,gear,species,FG,Keep_Atlantis) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

### Non-IFQ species in catch share sectors
# definitely want to double-check the catch categories here
dat_nonifq_atl <- dat_nonifq %>%
  left_join(spp_match,by=c('species','scientific_name')) %>%
  # fill in zeroes
  mutate(ret_mt=replace_na(ret_mt,0),
         total_est_dis_mt=coalesce(total_est_dis_mt,dis_ob_mt)) %>% 
  mutate(total_est_dis_mt=replace_na(total_est_dis_mt,0)) %>% 
  mutate(ret_dt_mt=ret_mt+total_est_dis_mt,
         year=as.integer(year)) %>%
  group_by(iopac,year,gear,species,FG,Keep_Atlantis) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

### All species, catch share sectors 2021
# definitely want to double-check the catch categories here
dat_allspp_cs_2021_atl<- dat_allspp_cs_2021 %>%
  left_join(spp_match,by=c('species','scientific_name')) %>%
  mutate(ret_mt=replace_na(ret_mt,0),
         total_est_dis_mt=replace_na(total_est_dis_mt,0)) %>% 
  mutate(ret_dt_mt=ret_mt+total_est_dis_mt,
         year=as.integer(year)) %>%
  group_by(iopac,year,gear,species,FG,Keep_Atlantis) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

# Organize output and save
# for this we do even more lumping, into 4 main categories- 
# 1. all non-tribal bottom trawl catch (non-IFQ and IFQ), by port group
# 2. all tribal trawl catch, lumped (Makah)
# 3. all tribal non-trawl catch, lumped
# 4. all other non-trawl catch, by port group

total_catch_summarized <- list(ifq=dat_ifq_atl,noncs=dat_allspp_noncs_atl,
                               nonifq=dat_nonifq_atl,cs2021=dat_allspp_cs_2021_atl,
                               tribal=dat_tribal_atl) %>% 
  bind_rows(.id='dataset') %>% 
  filter(!is.na(FG),FG!="Reallocate",FG!="Remove")

unique(total_catch_summarized$gear)

# bottom trawl all species
bottom_trawl_port_year <- total_catch_summarized %>% 
  filter(gear=="Bottom Trawl",dataset!='tribal') %>% 
  group_by(iopac,year,species,FG,Keep_Atlantis) %>% 
  summarise(totcatch=sum(totcatch)) %>% 
  ungroup() %>% 
  # dplyr::select(-dataset) %>% 
  mutate(fishery_type="bottom trawl")

# tribal
tribal_trawl_year <- total_catch_summarized %>%
  # pull tribal trawl data (any kind of trawl)
  filter(dataset=="tribal",grepl("Trawl",gear)) %>% 
  group_by(iopac,year,species,FG,Keep_Atlantis) %>% 
  summarise(totcatch=sum(totcatch)) %>% 
  mutate(fishery_type="tribal trawl") %>% 
  ungroup()

# tribal non-trawl
tribal_nontrawl_year <- total_catch_summarized %>%
  # pull tribal trawl data (any kind of trawl)
  filter(dataset=="tribal",!grepl("Trawl",gear)) %>% 
  group_by(iopac,year,species,FG,Keep_Atlantis) %>% 
  summarise(totcatch=sum(totcatch)) %>% 
  mutate(fishery_type="tribal non-trawl") %>% 
  ungroup()

# all other non-trawl, non-tribal catch (generic fleets)
other_gears_port_year <- total_catch_summarized %>% 
  filter(gear!="Bottom Trawl",dataset!='tribal') %>% 
  group_by(iopac,year,species,FG,Keep_Atlantis) %>% 
  summarise(totcatch=sum(totcatch))%>% 
  mutate(fishery_type="generic") %>% 
  ungroup()


### PLOTS AND CHECKS ###
# bottom trawl with or without "other" species
trawl_yes_no <- bottom_trawl_port_year %>% 
  filter(year==2013) %>% 
  complete(FG,Keep_Atlantis,fill=list(totcatch=0)) %>% 
  group_by(FG,Keep_Atlantis) %>% 
  summarise(totFGcatch=sum(totcatch)) %>% 
  ungroup() %>% 
  group_by(FG) %>% 
  mutate(propcatch=totFGcatch/sum(totFGcatch)) %>% 
  ungroup() %>% 
  mutate(FGfct=factor(FG))

trawl_yes_no %>% 
  ggplot(aes(FG,propcatch,fill=Keep_Atlantis,col=Keep_Atlantis))+
  geom_col()+
  theme_minimal()+
  labs(x="Functional Group",y="Proportion of Total Catch",title="Bottom Trawl")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=0.7))
# problematic groups are c('FBP','FDE','FDO','FMN','FMM','FPS','FVT','SHR','SSK')
# look manually at species catches in these groups
x <- bottom_trawl_port_year %>% 
  filter(year==2013) %>% 
  group_by(FG,species,Keep_Atlantis) %>%
  summarise(totcatch=sum(totcatch)) %>% 
  filter(FG %in% c('FBP','FVT','SHR')) %>% 
  arrange(FG,Keep_Atlantis,desc(totcatch))

###

makah_yes_no <- tribal_trawl_year %>% 
  filter(year==2013) %>% 
  complete(FG,Keep_Atlantis,fill=list(totcatch=0)) %>% 
  group_by(FG,Keep_Atlantis) %>% 
  summarise(totFGcatch=sum(totcatch)) %>% 
  ungroup() %>% 
  group_by(FG) %>% 
  mutate(propcatch=totFGcatch/sum(totFGcatch)) %>% 
  ungroup() %>% 
  mutate(FGfct=factor(FG))

makah_yes_no %>% 
  ggplot(aes(FG,propcatch,fill=Keep_Atlantis,col=Keep_Atlantis))+
  geom_col()+
  theme_minimal()+
  labs(x="Functional Group",y="Proportion of Total Catch",title="Tribal Trawl")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=0.7))
# problematic groups are c('FDB','FDE','FDO','FPS','SHR','SSK')
x2<-tribal_trawl_year %>% 
  filter(year==2013) %>% 
  group_by(FG,species,Keep_Atlantis) %>%
  summarise(totcatch=sum(totcatch)) %>% 
  filter(FG %in% c('FDB','FDE','FDO','FPS','SHR','SSK')) %>% 
  arrange(FG,Keep_Atlantis,desc(totcatch))

###
tribal_nontrawl_yes_no <- tribal_nontrawl_year %>% 
  filter(year==2013) %>% 
  complete(FG,Keep_Atlantis,fill=list(totcatch=0)) %>% 
  group_by(FG,Keep_Atlantis) %>% 
  summarise(totFGcatch=sum(totcatch)) %>% 
  ungroup() %>% 
  group_by(FG) %>% 
  mutate(propcatch=totFGcatch/sum(totFGcatch)) %>% 
  ungroup() %>% 
  mutate(FGfct=factor(FG))

tribal_nontrawl_yes_no %>% 
  ggplot(aes(FG,propcatch,fill=Keep_Atlantis,col=Keep_Atlantis))+
  geom_col()+
  theme_minimal()+
  labs(x="Functional Group",y="Proportion of Total Catch",title="Tribal Non-Trawl")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=0.7))
# problematic groups are c('FDB','FDD','FDE','FDO','FDS','FMM','SSK')
x3<-tribal_nontrawl_year %>% 
  filter(year==2013) %>% 
  group_by(FG,species,Keep_Atlantis) %>%
  summarise(totcatch=sum(totcatch)) %>% 
  filter(FG %in% c('FDB','FDD','FDE','FDO','FDS','FMM','SSK')) %>% 
  arrange(FG,Keep_Atlantis,desc(totcatch))

###

generic_yes_no <- other_gears_port_year %>% 
  filter(year==2013) %>% 
  complete(FG,Keep_Atlantis,fill=list(totcatch=0)) %>% 
  group_by(FG,Keep_Atlantis) %>% 
  summarise(totFGcatch=sum(totcatch)) %>% 
  ungroup() %>% 
  group_by(FG) %>% 
  mutate(propcatch=totFGcatch/sum(totFGcatch)) %>% 
  ungroup() %>% 
  mutate(FGfct=factor(FG))

generic_yes_no %>% 
  ggplot(aes(FG,propcatch,fill=Keep_Atlantis,col=Keep_Atlantis))+
  geom_col()+
  theme_minimal()+
  labs(x="Functional Group",y="Proportion of Total Catch",title="Generic Fleets")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=0.7))
# problematic groups are c('FDB','FDD','FDE','FDF','FDO','FPS','FVT','SHB','SHR','SSK')
x4<-other_gears_port_year %>% 
  filter(year==2013) %>% 
  group_by(FG,species,Keep_Atlantis) %>%
  summarise(totcatch=sum(totcatch)) %>% 
  filter(FG %in% c('FDB','FDD','FDE','FDF','FDO','FPS','FVT','SHB','SHR','SSK')) %>% 
  arrange(FG,Keep_Atlantis,desc(totcatch))

# palfleets <- viridis::viridis_pal(option="D")(3) %>% set_names(c('bottom trawl','tribal','generic'))

catch2013 <- bind_rows(bottom_trawl_port_year,tribal_trawl_year,tribal_nontrawl_year,other_gears_port_year) %>% 
  filter(year==2013) %>% 
  #final removal of unused species
  filter(Keep_Atlantis=="Y") %>% 
  group_by(iopac,year,FG,fishery_type) %>% 
  summarise(totcatch=sum(totcatch,na.rm=T)) %>% 
  ungroup()

## UPDATE 08.05.2022## add CPS, Canadian, and Mexican catches
# catch data cps
dat_cps <- read_csv(here('data','atlantis','PacFIN_year_atlantis_cps.csv'))
pacfin_spp_FG<- tibble(PACFIN_SPECIES_CODE=c("NANC","OMCK","PSDN","MSQD"),
                       FG=c("ANC","FPL","SAR","MSQ"))
# add cps to overall catch dataset
dat_cps <- dat_cps %>% 
  left_join(pacfin_spp_FG,by="PACFIN_SPECIES_CODE") %>% 
  mutate(fishery_type="CPS") %>% 
  rename(iopac=PORT_AREA_CODE,year=LANDING_YEAR,totcatch=LANDINGS) %>% 
  filter(year==2013) %>% 
  dplyr::select(iopac,year,FG,fishery_type,totcatch)

catch2013 <- catch2013 %>% bind_rows(dat_cps)

# catch data Canada/Mexico
# need to split by CPS and non-CPS
dat_cnmx <- read_csv(here('data','atlantis','catch_all_canada_mexico.csv')) %>% 
  set_names(c("source","grp","FG","totcatch"))
cnmx_cps <- dat_cnmx %>% 
  filter(source %in% c("Canada","Mexico"),
         FG %in% c("SAR","ANC","MSQ","FPS","FPL"),
         totcatch>0) %>% 
  dplyr::select(source,FG,totcatch) %>% 
  complete(source,FG,fill=list(totcatch=0)) %>%
  rename(iopac=source) %>% 
  distinct() %>% 
  mutate(fishery_type="CPS")

cnmx_generic <- dat_cnmx %>% 
  filter(source %in% c("Canada","Mexico"),
         !(FG %in% c("SAR","ANC","MSQ","FPS","FPL")),
         totcatch>0) %>% 
  dplyr::select(source,FG,totcatch) %>% 
  complete(source,FG,fill=list(totcatch=0)) %>%
  rename(iopac=source) %>% 
  distinct() %>% 
  mutate(fishery_type="generic")

catch2013=catch2013 %>% 
  bind_rows(cnmx_cps) %>% 
  bind_rows(cnmx_generic)

### WRITE ###

write_csv(catch2013,here::here('data','atlantis','catch_by_fleet_2013.csv'))
