###############################################
###############################################
######### Reshaping all Barb's SDMs
###############################################
###############################################

library(sf)
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(ggplot2)
library(dplyr)

#######
## Loading polygons (Owen, you may have the polygon file in your toolbox)

polyCalCurr<-read.table("/home/hernvann/ERDDAP_extractions/CalCurrentV3PolygonCoordsPBS.csv",as.is = TRUE,header=TRUE,sep=",") 
polyCalCurr$X<-polyCalCurr$X-360
polyCalCurr<-st_as_sf(polyCalCurr, coords = c("X", "Y"), agr = "PID", crs = 4326)
polyCalCurr$PID<-as.numeric(polyCalCurr$PID)
for(r in 0:(length(unique(polyCalCurr$PID))-1)){
  geometry<-polyCalCurr[which(polyCalCurr$PID==r),]
  res<-st_sf(st_cast(st_combine(geometry),"POLYGON"))
  res<-cbind(res,r)
  if(r==0){
    polys<-res
  }else{
    polys<-rbind(polys,res)
  }
}

#######
## Defining the key variables

# defining the time-range for which we will formate the distributions
year_seq <- seq(2013,2099)
start_proj_y <- 2020 
month_sel <- seq(1,12)
##### !!!! We still don't have the SDM projections up to 2099 - So just focus in the 2013-2019 period.
# We will use seq(2013,2099) later
year_seq <- seq(2013,2019)

allsp <- unique(selecSDM$name)
abrev <- c("anch","chub","herr","jack","sard","squid")

# set the repository in which you store your SDM outputs
sp_SDM <- list.files("SDM_outputs/Muhling_SDMs/")

# defining indices that will be used in the script - sorry, not very clean
common_name_SDM <- c("anchovy","chub mackerel","herring","jack mackerel","sardine","market squid")
sp_SDM_gp <- c("ANC","FPL","HER","JAC","SAR","MSQ")
lfstg_SDM_gp <- c("Ad","Ad","Ad","Ad","Ad","Ad")
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Here we treat seasonal distributions; we need to calculate a few metrics that will inform the nc file used by Atlantis
SDM_timestep <- 86400 #number of seconds in 1 day
month_start_day <- SDM_timestep + seq(from=0, by=3600*24*364/12, length.out = 12)
month_start_day_ally <- rep(seq(0,(length(year_seq)-1))*3600*24*365, each=12) + rep(month_start_day, length(year_seq))

# Here we load a txt file that will be the base brick for creating the nc files
buildblock <- readLines("SDM_buildingblock.txt")

# I get several types of models; not very relevant for you, Owen
rep_cat <- c("", "_spawning")
#rep_cat <- c("hindcast", "hindcast_spawning")


for(pred_class in rep_cat){
  
  # identifying the species for which we have SDMs
  sp_SDM <- list.files(paste0("SDM_outputs/Muhling_SDMs/hindcast", pred_class))
  
  for(sp in sp_SDM){
    
    #base bricks for the densities and the time indices that will be included in the nc file 
    distri_text_all_mth <- NA
    all_time_index <-  NA
    all_time_steps <- NA
    
    #we proceed year by year
    for(y in year_seq){
      
      # the SDM outputs, depending on the year, may be extracted from the hindasting SDMs or the forecasting SDMs
      pred_type <- ifelse(y > start_proj_y, "forecast", "hindcast")
      
      if(dir.exists(paste0("SDM_reshaped/Muhling_SDMs/SDMs_time-series/", gsub("_","",pred_class)))==F){
        dir.create(paste0("SDM_reshaped/Muhling_SDMs/SDMs_time-series/", gsub("_","",pred_class)))
      } 
    
    # we replace basic info for the nc file  
    FG_code_replace <- sp_SDM_gp[which(sp_SDM==sp)]
    FG_name_replace <- common_name_SDM[which(sp_SDM==sp)]
    LifeStage_name_replace <- lfstg_SDM_gp[which(sp_SDM==sp)]
    LifeStage_code_replace <- ifelse(LifeStage_name_replace=="Ad", "2", "1")
    SDM_nc <- buildblock
    SDM_nc[grep("LifeStage_code",SDM_nc)] <- gsub("LifeStage_code", LifeStage_code_replace, grep("LifeStage_code", SDM_nc, value=T ))
    SDM_nc[grep("FG_code",SDM_nc)] <- gsub("FG_code", FG_code_replace, grep("FG_code", SDM_nc, value=T ))
    SDM_nc[grep("LifeStage_name",SDM_nc)] <- gsub("LifeStage_name", LifeStage_name_replace, grep("LifeStage_name", SDM_nc, value=T ))
    SDM_nc[grep("FG_name",SDM_nc)] <- gsub("FG_name", FG_name_replace, grep("FG_name", SDM_nc, value=T ))
    SDM_nc[grep("SDM_timestep",SDM_nc)] <- gsub("SDM_timestep", SDM_timestep, grep("SDM_timestep",SDM_nc, value=T ))
    
    # In past work, Barbara provided several model types - now we only have one
    model_folder <- list.files(paste0("SDM_outputs/Muhling_SDMs/", pred_type, pred_class, "/", sp))
    mod <- model_folder[1]

    # We have from Barb 1 output file per month/year; Let's select the outputs corresponding to the year of interest
      allouty <- list.files(paste0("SDM_outputs/Muhling_SDMs/", pred_type, pred_class, "/", sp, "/", mod)) 
      allouty <- unlist( lapply (y, FUN=function(x){grep (x, allouty, value=T)}) ) 
      
      SDM_nc_mod <- SDM_nc

      for (mth in month_sel){
        
        # just check that this month is available for this year (some projections don't end in December)
        if(length(grep( paste0("_", mth, "_"),  allouty))>0){
        
          # Load the SDM output file
        allout_mth <- grep( paste0("_", mth, "_"),  allouty, value=T)
        rasters_day <- brick(paste0("SDM_outputs/Muhling_SDMs/", pred_type, pred_class, "/", sp, "/", mod, "/", allout_mth))
        # the file includes the predictions per day - let's take the mean to get a distribution for the whole month
        rasters_day_mean <- mean(rasters_day, na.rm=T)
        file_expl <- rasters_day_mean
        
        # Extract the mean value per polygon
        polyextr <- cbind.data.frame(r=seq(0,88), relB = raster::extract(file_expl, polys, fun=mean, na.rm =TRUE) )
        polyextr_sf <- inner_join(polys, polyextr, left=T)
        #Calculate the contribution of each polygon to the total biomass
        polyextr_sf %>%
          dplyr::mutate(relB = as.numeric(relB/max(relB, na.rm=T))) %>%
          dplyr::mutate(surf = as.numeric(st_area(st_cast.st_combine.geometry....POLYGON..))) %>%
          dplyr::mutate(B = as.numeric(surf * relB) ) %>%
          dplyr::mutate(prop_Btot = as.numeric(B/sum(B,na.rm=T))) -> polyextr_sf 
        
        #store the data for all polygons
        distri_text <- paste0(paste(format(round(polyextr_sf$prop_Btot, 4), nsmall = 4), collapse = ", "), ",")
        # and integrate at the following of the sitributions for all previous years/months
        distri_text_all_mth <- c(distri_text_all_mth, distri_text)

        }
      }
      

    }
    
    # basic replacement of info in the nc file
    SDM_nc_mod[grep("timestep_list",SDM_nc_mod)] <- gsub("timestep_number_list",
                                                                paste0(month_start_day_ally, collapse=", "),
                                                                SDM_nc_mod[grep("timestep_number_list",SDM_nc_mod)])
    SDM_nc_mod[grep("timestep_number_list",SDM_nc_mod)] <- gsub("timestep_list",
                                                         paste0(seq(1,12*length(year_seq)), collapse=", "),
                                                         SDM_nc_mod[grep("timestep_list",SDM_nc_mod)])

    distri_text_all_mth <- distri_text_all_mth[-1]
    comment_SDM <- paste0("- SDM from Muhling (pers. comm.); type=", mod, sep = "")
  
    SDM_nc_mod[grep(":title", SDM_nc_mod)] <- gsub(paste(FG_name_replace, LifeStage_name_replace),
                                                   paste(paste(FG_name_replace, LifeStage_name_replace), comment_SDM),
                                                   SDM_nc_mod[grep(":title", SDM_nc_mod)])
    
    distri_text_all_mth[length(distri_text_all_mth)] <- paste0(substr(distri_text_all_mth[length(distri_text_all_mth)],
                                                                      1,
                                                                      (nchar(distri_text_all_mth[length(distri_text_all_mth)])-1)),
                                                               ";")
    
    insert_distri <- grep("distrilines", SDM_nc_mod)
    
    # Integration of the biomass distri info in the file
    SDM_nc_mod_write <- c(SDM_nc_mod[1:(insert_distri - 1)],
                          distri_text_all_mth,
                          SDM_nc_mod[(insert_distri + 1):length(SDM_nc_mod)])
    
    # store the nc files
    write(SDM_nc_mod_write,
          file=paste0("SDM_reshaped/Muhling_SDMs/SDMs_time-series/", gsub("_","",pred_class), "/", FG_code_replace, "_distrib", LifeStage_code_replace, ".cdf"), append=TRUE)
    
  }
  
}
