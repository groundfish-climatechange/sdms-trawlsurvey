##---------------------------------------------------------------##
##----  Quantifying novel space in the environmental niche   ----##
##----  ... with implications for SDM projection and ...     ----##
##----  ... emerging climate impacts                         ----##
##----  Example Version                                      ----##
##----  J.A.Smith - UCSC/NOAA - Dec 2021 (R v4.1.0)          ----##
##---------------------------------------------------------------##

## Notes on hypervolume package:
## - Best to center and scale all variables; this requires a global mean and SD...
## ... I calculate these from my historical reference set (for each ESM) and use them for all future data.
## - For trajectories of future novelty, I use an 'inclusion test', which requires...
## ... a data.frame to build the historical hypervolume (in an SDM context, your observations), and...
## ... rasters for your future period (in an SDM context, your prediction data).
## - Hypervolumes are computationally intense, and more than 6 climate variables and ~300K observations...
## ... is asking for trouble re: fitting. For me, this meant subsetting 5% of my 30y historical...
## ... data at the native ROMS resolution (using monthly means).
## - There are a couple of algorithms to delineate the hypervolume, but I prefer support vector machine...
## ... mainly because it fits around the around the data very closely, and indicates...
## ... whether future data are 'inside or outside' historical (rather than a probability).

## *** MAKE SURE YOU DO ONE ESM/MODEL AT A TIME - the means and SDs are specific to each historical data set...
## ... and these will change if the historical data set changes

## "Historical" and "Future" are the relevant terms for measuring climate envelope novelty, but can be exchanged...
## with "Observations" and "Predictions" when measuring SDM extrapolation


## EDITED BY OWEN LIU, 05/26/2022
library(tidyverse)
library(hypervolume)
library(terra)
library(here)

## NOTE: SEEING IF HYPERVOLUME CODE WILL RUN FASTER IF WE REPLACE SOME FUNCTIONS WITH TERRA INSTEAD OF RASTER:
hypervolume_project <- function (hv, rasters, type = "probability", verbose = TRUE,...){
  raster.values = data.frame(terra::values(rasters))
  na.rows <- raster.values %>% rowid_to_column("idx") %>% filter(if_any(everything(),is.na)) %>% pull(idx)
  if (type == "probability") {
    projected.values <- hypervolume_estimate_probability(hv = hv, 
                                                         points = raster.values, verbose = verbose, ...)
  }
  else if (type == "inclusion") {
    projected.values = hypervolume_inclusion_test(hv, raster.values, 
                                                  verbose = verbose, ...)
  }
  else {
    stop("Unsupported 'type' argument")
  }
  
  raster.out <- rasters[[1]]
  if (length(attr(projected.values, "NAIdx")) > 0) {
    values(raster.out)[-na.rows] <- projected.values
    values(raster.out)[na.rows] <- NA
  }
  else {
    values(raster.out) <- projected.values
  }
  return(raster.out)
}

## Identify the environmental variables which define your climate/model niche 
## --------------------------------------------------------------------------
## ... for SDMs, what you fit the SDM to; excluding space-time variables
## ... these must be headers in your data files AND your future raster stack MUST BE IN THE SAME ORDER
## You can also add lat and lon here, which then measures novel locations (important for some SDMs)...
## ... and novel conditions at locations (i.e. a novely warm SST for the PNW)

env_vars <- c("sst","ild","oxygen")


## Load and scale the 'historical' data (observations)
## ---------------------------------------------------

hist_data <- read_rds(here('data','hypervolumes',"hist_data_example.rds")) %>% 
  ungroup()#an example subset from IPSL 1980-2009

means <- hist_data %>% summarise(across(env_vars,mean))#'global' values used to standardise all your data
SDs <- hist_data %>% summarise(across(env_vars,sd))

hist_data_s <- hist_data %>% dplyr::select(env_vars) %>% scale(center=means,scale=SDs) %>% as_tibble()

## Subset data *only if required*
## -----------------------
## ...subset so you have < 300K observations; but trial this so your hypervolume fits in ~30 mins

set.seed(123)
prop <- 0.5  
hist_data_s <- hist_data_s %>% slice_sample(prop=prop,replace = F)
head(hist_data_s)

## Build historical hypervolume
## ----------------------------
## ... should take 5-60 mins for large data

hvh = hypervolume(data=hist_data_s,
                  method='svm')  #there are a couple of options in here, but I've found the defaults fine

# summary(hvh)  #some generic info
# get_volume(hvh)
# get_centroid(hvh)
plot(hvh, show.3d=F)  #pair plots of environmental niche; dark red = data subset; small red = random points inside hypervolume


## Create/load future rasters and standardise
## ----------------------------------------------

r_sst <- rast(here('data','hypervolumes',"sst_ipsl_7_2080.grd"))  #example month: July 2080 IPSL
r_ild <- rast(here('data','hypervolumes',"ild_ipsl_7_2080.grd"))
r_oxygen <- rast(here('data','hypervolumes',"oxygen_ipsl_7_2080.grd"))

rx <- c(r_sst,r_ild,r_oxygen)
names(rx) <- c("sst","ild","oxygen")  #*MUST match 'env_vars' (same order as fitted hypervolume)

rx$sst <- scale(rx$sst, center=pull(means,'sst'), scale=pull(SDs,'sst'))  #center and scale using global values
rx$ild <- scale(rx$ild, center=pull(means,'ild'), scale=pull(SDs,'ild'))
rx$oxygen <- scale(rx$oxygen, center=pull(means,'oxygen'), scale=pull(SDs,'oxygen'))


## Calculate inclusion of these future raster values in historical hypervolume
## --------------------------------------------------------------------

hyp_proj <- hypervolume_project(hvh, rasters=rx,
                                type="inclusion",
                                fast.or.accurate="accurate")

excl <- sum(values(hyp_proj)==0,na.rm = T)  #number of cells 'excluded'; in example ~3000 cells
excl_prop <- excl/sum(values(hyp_proj)>=0,na.rm=T) #proportion of cells excluded; in example ~14%

# *** ^ save this proportion for this time step, this is what is plotted against time...
# ... i.e. the proportion of the pixels (i.e. area) that are novel (excluded from the hypervolume) through time...
# ... I have the 'load future rasters and inclusion test' steps inside a loop, and save 'excl_prop' inside...
# ... a data.frame for each time step

plot(hyp_proj, asp=1)  #this is the inclusion raster; green = included (analog conditions), white = excluded (novel conditions)

# *** ^ I also save each time step's raster (hyp_proj) for later visualisation


## END ------------------------------------------------------------------

