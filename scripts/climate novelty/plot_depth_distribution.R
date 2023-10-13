# Depth distribution change for projected SDMs
library(tidyverse)

# assuming these models have been summarized using functions similar to those in `sdms_elasmo_flatfish.Rmd`
# we compare the cumulative depth distribution of species between a 1985-2010 baseline and the 2075-2100 period

#function
plot_depth_distribution <- function(projection_df){
  historical_depth_distribution <- projection_df %>% 
    filter(year>1984,year<2011) %>%
    mutate(cpue=exp(est)) %>% 
    rename(depth=depth_m) %>%
    mutate(depth=-depth) %>% 
    group_by(esm,depth) %>% 
    summarise(cpue=mean(cpue,na.rm=T)) %>% 
    ungroup() %>% 
    group_by(esm) %>% 
    mutate(prop_cpue=cpue/sum(cpue,na.rm=T)) %>%
    arrange(desc(depth)) %>% 
    mutate(cum_cpue=cumsum(prop_cpue)) %>% 
    ungroup() %>% 
    mutate(period='1985-2010')
  
  future_depth_distribution <- projection_df %>% 
    filter(year>2074,year<2101) %>%
    mutate(cpue=exp(est)) %>% 
    rename(depth=depth_m) %>% 
    mutate(depth=-depth) %>% 
    group_by(esm,depth) %>% 
    summarise(cpue=mean(cpue,na.rm=T)) %>% 
    ungroup() %>% 
    group_by(esm) %>% 
    mutate(prop_cpue=cpue/sum(cpue,na.rm=T)) %>%
    arrange(desc(depth)) %>% 
    mutate(cum_cpue=cumsum(prop_cpue)) %>% 
    ungroup() %>% 
    mutate(period='2075-2100')
  
  plot_out <- historical_depth_distribution %>% 
    bind_rows(future_depth_distribution) %>% 
    ggplot(aes(depth,cum_cpue,col=factor(period)))+
    geom_line(size=2)+
    xlim(-2000,0)+
    # 700 fathom line
    geom_vline(xintercept=-1280.16,linetype=2)+
    coord_flip()+
    facet_wrap(~esm)+
    scale_color_manual(values=c("#2271B2","#d55e00"))+
    labs(x="Depth (m)",y="Cumulative Proportion",title="",col="Period")
  
  plot_out
}

# a couple of examples...
dogfish <- read_rds(here('Janelle','models_owen','pacific spiny dogfish','projection_3ESMS.rds'))
longnose <- read_rds(here('Janelle','models_owen','longnose skate','projection_3ESMS.rds'))
arrowtooth <- read_rds(here('Janelle','models_owen','arrowtooth flounder','projection_3ESMS.rds'))

plot_depth_distribution(dogfish)
plot_depth_distribution(longnose)
plot_depth_distribution(arrowtooth)
