# quick footprints map
test <- calc_footprint_reldens_ts(sable_ens_all,what='df')
test2 <- test %>% ggplot(aes(year,perc_change))+geom_line()+
  facet_wrap(~port_name,ncol=1)+theme_minimal()+
  geom_hline(yintercept=0,linetype=2)+
  labs(x="Year",y="CPUE Change (%)")

test3 <- plot_grid(test2,NULL,footprints_map,ncol=3,rel_widths = c(1,-0.1,1))
test3
test4 <- ggdraw()+
  draw_plot(test3)+
  draw_image(here('data','icons','sablefish_sil.png'),hjust=-0.17,vjust=0.35,scale=0.25)
test4
ggsave(here('model output','dts paper','fig_sable_cpue_footprints.png'),test4,h=6,w=5)
       