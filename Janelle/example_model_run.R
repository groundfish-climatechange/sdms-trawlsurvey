---
title: "example model run"
author: "Owen Liu"
date: '2022-06-13'
output: html_document
---
# Example sdmTMB run with NWFSC trawl data
```{r}
library(tidyverse)
```


# Load Data

```{r}
rmarkdown::render(here::here('scripts','sdmTMB data construction.Rmd'),quiet=TRUE)
```

# sdmTMB Model Function

```{r}
prepare_species <- function(dat,spp){
  dat_sub <- dat %>% 
    filter(species==spp) %>% 
    
    # rescale depth, oxygen, and temp to be N(0,1)
    mutate(across(c(depth_trawl,mean_temp_roms_30,mean_oxygen_roms_30),~(scale(.) %>% as.vector()),.names="{.col}_norm")) %>% 
    
    # add a year indicator
    mutate(year=lubridate::year(date))
}
```


```{r}
run_sdmTMB <- function(dat,spp,nknots=400,use_depth=F,time_vary=F,spatial_field=T,include_substrate=T,hab_spline=F,env_spline=F,spline_k=3){
  # filter data for species
  modeldat <- prepare_species(dat,spp=spp)
  
  # make spde
  spde <- make_mesh(modeldat,xy_cols = c('longitude','latitude'), 
                   cutoff = 20)
  
  # model formula
  formula <- paste0("cpue_kg_km2 ~ ")
  
  # substrate relationship
  substrate <- paste("prop_hard_mixed + I(prop_hard_mixed^2)")
  #wiggly habitat relationship?
  substrate <- ifelse(hab_spline, paste0("s(prop_hard_mixed,k=",spline_k,")"),
                      substrate)
  if(!include_substrate) substrate=""
  
  # make the environmental effects
  enviro <- paste("mean_temp_roms_30_norm + 
                  I(mean_temp_roms_30_norm^2) + 
                  mean_oxygen_roms_30_norm + 
                  I(mean_oxygen_roms_30_norm^2)")
  # wiggly environmental relationships?
  enviro <- ifelse(env_spline, paste0("s(mean_temp_roms_30_norm,k=",spline_k,") + ",
                                      "s(mean_oxygen_roms_30_norm,k=",spline_k,")"),
                   enviro)
  # if depth effect, add to model formla
  if(use_depth) {
    formula = paste0(formula, " + depth + I(depth^2)")
  }
  
  time_formula = "~ -1"
  if(time_vary) {
    time_formula = paste0(time_formula, " + ", substrate, " + ", enviro)
    time_varying = as.formula(time_formula)
    time = "year"
  } else {
    formula = paste0(formula, " + ", substrate, " + ", enviro)
    time_varying = NULL
    time = "year"
  }
  
  # fit model. EW commented out quadratic roots, since those are still experimental and won't work for all spp. Also turned
  # set.seed(41389) # for reproducibility
  # test_set = sample(1:nrow(modeldat), size = round(0.1*nrow(modeldat)), replace=FALSE)
  # modeldat$fold = 1
  # modeldat$fold[test_set] = 2
  # anisotropy off for now
  print('running model.')
  m <- try( sdmTMB(
    formula = as.formula(formula),
    time_varying = time_varying,
    spde = spde,
    time = time,
    family = tweedie(link = "log"),
    data = modeldat,
    anisotropy = FALSE,
    spatial_only = T,
    #extra_time argument necessary for prediction?
    extra_time=1980:2100,
    control=sdmTMBcontrol(map_rf=ifelse(spatial_field,F,T))
  ),
  silent=F)


  # predicted values for the 2nd fold (test)
  # m_cv$data$cv_predicted[which(m_cv$data$cv_fold==2)]
  # log likelihood values for the 2nd fold (test)
  # m_cv$data$cv_loglik[which(m_cv$data$cv_fold==2)]

    # sum(m_cv$data$cv_loglik[which(m_cv$data$cv_fold==2)])
  
  # if(class(m)!="try-error") {
  #   write_rds(m, file=here::here('model output',
  #                                paste0(spp,'.rds')))
  # }
  if(class(m)=="try-error"){
    print(paste("Error."))
  }else{
    print(paste("Model for",spp,"complete."))
  }

  # return(m)
  return(m)

}
```

# Choose a species!