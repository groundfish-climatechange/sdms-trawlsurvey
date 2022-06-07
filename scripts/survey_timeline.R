## Quick timeline figure for trawl survey overlap
library(tidyverse)
library(viridis)
# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=10),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=12,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

survey <- tibble(x=c(2003,2003,2022,2022),y=c(0,1,1,0),id="trawl survey",lt=1)
ccroms <- tibble(x=c(1980,1980,2010,2010),y=c(0,1,1,0),id="CCROMS hindcast",lt=2)
# overlap <- tibble(x=c(2003,2003,2010,2010),y=c(0,1,1,0),id="fitting",lt=NA)

polys = bind_rows(survey,ccroms) %>% 
  mutate(id=factor(id,levels=c("CCROMS hindcast","trawl survey"))) %>% 
  mutate(lt=as.factor(lt))

timeline=ggplot(polys,aes(x,y))+
  geom_polygon(aes(fill=id,group=id,linetype=lt),alpha=0.5,size=1.5,color="black")+
  labs(x="Year",y="",fill="")+
  guides(linetype='none')+
  annotate("text",x=2006,y=0.5,label="fitting",angle=45,size=5)+
  annotate("text",x=2015,y=0.5,label="projection\u2B62",size=5)+
  scale_fill_manual(values=viridis_pal()(2))+
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y=element_blank())
timeline
