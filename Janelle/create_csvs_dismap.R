# re-form .csvs for John Kennedy
# for integration into the DisMAP portal

library(tidyverse)
spp_lookup <- read_rds(here::here('data','trawl','nwfsc_species_lookup.rds'))
# function to re-sort existing output from model projections

# In the example .csv sent to me (Owen) by John, the .csv looked like the following:
# (my understanding is that unneeded columns can be blank)
# columns:
# "blank" (row numbers)
# region (West Coast SDMs??)
# haulid
# year (integer)
# spp (scientific, character, capitalized)
# wtcpue (for us, our estimated density, numeric double)
# common (common name, character, capitalized)
# stratum
# stratumarea
# lat (decimal degrees, out to 7 decimal points, numeric double)
# lon (decimal degrees, out to 7 decimal points, numeric double)
# depth (in meters, positive integer)


# example file for testing the function
longnose_file<-here::here('Janelle','models_owen','longnose skate','projection_3ESMs.rds')
# provide 
form_dismap_csv <- function(full_filename_in,commonname,file_dir_out){
  dat <- read_rds(full_filename)
  sci <- spp_lookup %>% filter(common_name==commonname) %>% slice(1) %>% pull(scientific_name)
  dat_out <- dat %>% 
    # pull out required variables
    dplyr::select(year,lat,lon,depth=depth_m,est,esm) %>% 
    # exponentiate the cpue estimate and rename wtcpue
    mutate(wtcpue=exp(est)) %>% 
    mutate(year=as.integer(year)) %>% 
    # add empty vars
    mutate(haulid=NA,stratum=NA,stratumarea=NA,region="West Coast SDM") %>% 
    # round depth
    mutate(depth=round(depth)) %>% 
    # fix names
    mutate(common=commonname %>% str_to_sentence,
           spp=sci %>% str_to_sentence) %>% 
    # select and reorder
    dplyr::select(region,haulid,year,spp,wtcpue,common,stratum,stratumarea,lat,lon,depth,esm) %>% 
    group_split(esm,.keep=F)
  
  # output file names (for 3 esms)
  fls <- paste0(file_dir_out,"/",commonname," ",unique(dat$esm),'.csv')
  
  # save
  options(scipen=1000)
  purrr::map2(dat_out,fls,write.csv)
}

form_dismap_csv(longnose_file,"longnose skate",here::here('Janelle','models_owen'))
