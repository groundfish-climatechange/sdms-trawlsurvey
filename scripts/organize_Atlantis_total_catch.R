# Data from Kate Richerson 07/10/2022
library(tidyverse)
library(here)

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

# Matching key
atlantis_groups <- read_csv(here('data','atlantis_demersal_groups.csv'),col_types = 'ccc')
# try to match by names
species_lookup <- read_rds(here('data','trawl','nwfsc_species_lookup.rds')) %>% 
  mutate(common_name=tolower(common_name))

###
# dat_ifq2 <- dat_ifq %>% 
#   mutate(sci=tolower(scientific_name)) %>% 
#   left_join(species_lookup,by=c('sci'='scientific_name'))
# # what did we miss? try to manually fix some errors (iterative)
# x <- dat_ifq2 %>% filter(is.na(common_name)) %>% 
#   distinct(species,scientific_name,sci,common_name)
# x
# species_lookup %>% filter(grepl("butter",common_name,ignore.case = T)) %>% distinct()
# some species to fix (not incl. 'Unid')
species_lookup_fix <- species_lookup %>% 
  add_row(common_name="bocaccio",scientific_name="sebastes paucispinus") %>% 
  add_row(common_name="butter sole",scientific_name="pleuronectes isolepis") %>% 
  add_row(common_name="english sole",scientific_name="pleuronectes vetulus") %>%
  add_row(common_name="greenstriped rockfish",scientific_name="sebastes elongates") %>%
  add_row(common_name="northern rockfish",scientific_name="sebastes polyspinis") %>% 
  add_row(common_name="rex sole",scientific_name="errex zachirus") %>% 
  add_row(common_name="rock sole",scientific_name="pleuronectes bilineatus") %>% 
  add_row(common_name="rougheye/blackspotted rockfish",scientific_name="sebastes aleutianus") %>% 
  add_row(common_name="rougheye/blackspotted rockfish",scientific_name="sebastes melanostictus") %>% 
  add_row(common_name="rougheye/blackspotted rockfish",scientific_name="sebastes melanostictus and s. aleutianus") %>% 
  add_row(common_name="shortraker/rougheye/blackspotted rockfish",scientific_name="sebastes borealis/aleutianus") %>% 
  add_row(common_name="silvergray rockfish",scientific_name="sebastes brevispinus") %>% 
  add_row(common_name="vermilion rockfish",scientific_name="sebastes miniatus")

# summarize
dat_ifq_atl <- dat_ifq %>% 
  mutate(sci=tolower(scientific_name)) %>% 
  left_join(species_lookup_fix,by=c('sci'='scientific_name')) %>% 
  left_join(atlantis_groups,by=c('common_name'='trawl_name')) %>% 
  mutate(ret_dt_mt=ret_mt+dis_mt,
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,code,group_name) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

# now for the other sectors
### Non-catch share sectors ###
# this one has multiple sectors and gear--- sum across all?
dat_allspp_noncs_atl <- dat_allspp_noncs %>% 
  filter(sector!="Tribal") %>% # remove tribal catch
  mutate(sci=tolower(scientific_name)) %>% 
  left_join(species_lookup_fix,by=c('sci'='scientific_name')) %>% 
  left_join(atlantis_groups,by=c('common_name'='trawl_name')) %>% 
  # fill in zeroes
  mutate(ret_mt=replace_na(ret_mt,0),
         est_dis_mt=replace_na(est_dis_mt,0)) %>% 
  mutate(ret_dt_mt=ret_mt+est_dis_mt, # estimate of discards using ratios
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,code,group_name) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

## Tribal fisheries
dat_tribal_atl <- dat_allspp_noncs %>% 
  filter(sector=="Tribal") %>% 
  mutate(sci=tolower(scientific_name)) %>% 
  left_join(species_lookup_fix,by=c('sci'='scientific_name')) %>% 
  left_join(atlantis_groups,by=c('common_name'='trawl_name')) %>% 
  # fill in zeroes
  mutate(ret_mt=replace_na(ret_mt,0),
         est_dis_mt=replace_na(est_dis_mt,0)) %>% 
  mutate(ret_dt_mt=ret_mt+est_dis_mt, # estimate of discards using ratios
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,code,group_name) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

### Non-IFQ species in catch share sectors
# definitely want to double-check the catch categories here
dat_nonifq_atl <- dat_nonifq %>% 
  mutate(sci=tolower(scientific_name)) %>% 
  left_join(species_lookup_fix,by=c('sci'='scientific_name')) %>% 
  left_join(atlantis_groups,by=c('common_name'='trawl_name')) %>% 
  # fill in zeroes
  mutate(ret_mt=replace_na(ret_mt,0),
         total_est_dis_mt=coalesce(total_est_dis_mt,dis_ob_mt)) %>% 
  mutate(ret_dt_mt=ret_mt+total_est_dis_mt,
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,code,group_name) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

### All species, catch share sectors 2021
# definitely want to double-check the catch categories here
dat_allspp_cs_2021_atl<- dat_allspp_cs_2021 %>% 
  mutate(sci=tolower(scientific_name)) %>% 
  left_join(species_lookup_fix,by=c('sci'='scientific_name')) %>% 
  left_join(atlantis_groups,by=c('common_name'='trawl_name')) %>% 
  mutate(ret_dt_mt=ret_mt+total_est_dis_mt,
         year=as.integer(year)) %>% 
  group_by(iopac,year,gear,code,group_name) %>% 
  # sum the total catch
  summarise(totcatch=sum(ret_dt_mt)) %>% 
  ungroup()

# Organize output and save
# for this we do even more lumping- all bottom trawl species (non-IFQ and IFQ) except tribal catch go into one category, 
# while all tribal catch is kept separate, and finally, all non-trawl catch goes into another category
# all categories are also organized by year and port group
total_catch_summarized <- list(ifq=dat_ifq_atl,noncs=dat_allspp_noncs_atl,
                               nonifq=dat_nonifq_atl,cs2021=dat_allspp_cs_2021_atl,
                               tribal=dat_tribal_atl) %>% 
  bind_rows(.id='dataset') %>% 
  filter(!is.na(group_name))
# bottom trawl all species
bottom_trawl_port_year <- total_catch_summarized %>% 
  filter(gear=="Bottom Trawl",dataset!='tribal') %>% 
  dplyr::select(-dataset) %>% 
  mutate(fishery_type="bottom trawl")
# tribal
tribal_port_year <- total_catch_summarized %>% 
  filter(dataset=="tribal") %>% 
  group_by(iopac,year,code,gear,group_name) %>% 
  summarise(totcatch=sum(totcatch)) %>% 
  mutate(fishery_type="tribal") %>% 
  ungroup()
# all other non-trawl, non-tribal catch (generic fleets)
other_gears_port_year <- total_catch_summarized %>% 
  filter(gear!="Bottom Trawl",dataset!='tribal') %>% 
  group_by(iopac,year,code,group_name) %>% 
  summarise(totcatch=sum(totcatch))%>% 
  mutate(fishery_type="generic") %>% 
  ungroup()


# plots
palfleets <- viridis::viridis_pal(option="D")(3) %>% set_names(c('bottom trawl','tribal','generic'))
catch2013 <- bind_rows(bottom_trawl_port_year,tribal_port_year,other_gears_port_year) %>% 
  filter(year==2013)

  
    # ggplot(aes(fct_reorder(group_name,desc(totcatch)),totcatch,fill=fishery_type))+
  # geom_col(position='dodge')+
  # scale_fill_manual(values=palfleets)+
  # # facet_wrap(~iopac,scales='free')+
  # labs(x="Species Group",y="2013 Total Catch",fill="Fishery",title="North WA Coast, 2013")+
  # theme_minimal()+
  # theme(axis.text.x = element_text(angle=90,vjust=0.5))

catch2013
# look at a quick plot
# dat_ifq_atl %>% 
#   filter(!is.na(group_name)) %>% 
#   ggplot(aes(year,totcatch,col=group_name))+
#   geom_line()+geom_point()+
#   scale_x_continuous(labels=seq(2010,2022,by=2),breaks=seq(2010,2022,by=2))+
#   labs(x="Year",y="Catch (MT)",color="Atlantis\nGroup",title="IFQ Catch Shares")+
#   facet_wrap(~iopac,scales='free')+
#   theme_minimal()
# 
# dat_allspp_noncs_atl %>% 
#   filter(!is.na(group_name)) %>% 
#   ggplot(aes(year,totcatch,col=group_name))+
#   geom_line()+geom_point()+
#   scale_x_continuous(labels=seq(2010,2022,by=2),breaks=seq(2010,2022,by=2))+
#   labs(x="Year",y="Catch (MT)",color="Atlantis\nGroup",title="All Non-CS Sectors")+
#   facet_wrap(~iopac,scales='free')+
#   theme_minimal()
# 
# 
# dat_nonifq_atl %>% 
#   filter(!is.na(group_name)) %>% 
#   ggplot(aes(year,totcatch,col=group_name))+
#   geom_line()+geom_point()+
#   scale_x_continuous(labels=seq(2010,2022,by=2),breaks=seq(2010,2022,by=2))+
#   labs(x="Year",y="Catch (MT)",color="Atlantis\nGroup",title="Non-IFQ Species, Catch Share sectors")+
#   facet_wrap(~iopac,scales='free')+
#   theme_minimal()
# 
# 
# dat_allspp_cs_2021_atl %>% 
#   filter(!is.na(group_name)) %>% 
#   mutate(group_name_fct=forcats::fct_reorder(group_name,desc(totcatch))) %>% 
#   ggplot(aes(group_name_fct,totcatch,fill=group_name))+
#   geom_col()+
#   labs(x="",y="Catch (MT)",fill="Atlantis\nGroup",title="All Species, Catch Share Sectors 2021")+
#   facet_wrap(~iopac,scales='free')+
#   theme_minimal()+
#   theme(axis.text.x=element_blank(),
#         legend.position='bottom')