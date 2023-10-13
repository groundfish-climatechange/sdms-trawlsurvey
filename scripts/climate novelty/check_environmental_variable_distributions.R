# check distribution of environmental variables and species distributions in fitted vs projected ROMS time periods
library(tidyverse)
# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=10,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

# Load data
rmarkdown::render(here::here('scripts','sdmTMB-data-construction.Rmd'),quiet=TRUE)

# find average by year

roms_tempcomp <- roms %>% 
  mutate(time_period=case_when(
    year<2003 ~ "Before",
    2003<=year & year<= 2010 ~ "Training Period",
    year>2010 ~ "After"
  )) %>% 
  ggplot(aes(mean_bt_30d_ipsl,fill=factor(time_period)))+
  geom_density(alpha=0.5,color=NA)+
  labs(x="Temperature",y="Kernel Density",fill="Time Period")+
  theme(legend.position = c(0.6,0.8))

roms_tempcomp

roms_oxycomp <- roms %>% 
  mutate(time_period=case_when(
    year<2003 ~ "Before",
    2003<=year & year<= 2010 ~ "Training Period",
    year>2010 ~ "After"
  )) %>% 
  ggplot(aes(mean_oxy_bottom_30d_ipsl,fill=factor(time_period)))+
  geom_density(alpha=0.5,color=NA)+
  labs(x="Oxygen",y="Kernel Density",fill="Time Period")+
  theme(legend.position = c(0.6,0.8))

roms_oxycomp

# lingcod
ling <- read_rds(here('model output','lingcod','projection_3ESMs.rds')) %>% 
  dplyr::select(year,lat,lon,latitude,longitude,est)
glimpse(ling)

ling_periods <- ling %>% 
  mutate(time_period=case_when(
    year<2003 ~ "Before",
    2003<=year & year<= 2010 ~ "Training Period",
    year>2010 ~ "After"
  )) %>% 
  mutate(cpue=exp(est))

ling_periods_density<-ling_periods %>% 
  ggplot(aes(cpue,fill=factor(time_period)))+
  geom_density(alpha=0.5,color=NA)+
  labs(x="CPUE/Density",y="Kernel Density",fill="Time Period")+
  theme(legend.position = c(0.6,0.8))
ling_periods_density
# this looks crazy (probably some nuts outliers)
ling_periods_density+xlim(0,400)

# look for spatial outliers
quantiles_by_period <- ling_periods %>% 
  group_by(time_period) %>% 
  reframe(value=fivenum(cpue)) %>% 
  mutate(quant=rep(c("min","lower-hinge","median","upper-hinge","max"),3))

quantiles_by_period

# take top 1% of the predicted data and map it
# 99% quantile across all years is ~535
ling_quant95 <- ling_periods %>% 
  filter(cpue>535)
ling_other <- ling_periods %>% 
  filter(cpue<=535) %>% 
  sample_n(100000)

spatial_outliers_plot <- ggplot()+
  geom_point(data=ling_other,aes(longitude,latitude),color='gray50')+
  geom_point(data=ling_quant95,aes(longitude,latitude,color=time_period))+
  labs(x="Lon",y="Lat",color="Time Period")
spatial_outliers_plot
