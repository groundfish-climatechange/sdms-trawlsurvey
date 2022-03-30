##########################################################################################################
# From Eric Ward 08/2021, modified by Owen Liu 
# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)

library(raster)
library(sf)
library(here)

# use 

# read in the grid cell data from the survey design
grid_cells = readxl::read_excel(here('data','grids',"Selection Set 2018 with Cell Corners.xlsx"))
coordinates(grid_cells) <- c("Cent.Long", "Cent.Lat")
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells <- spTransform(grid_cells, CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))

# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)
# CHANGED TO KM
predict_raster = raster::raster(grid_cells, resolution = c(2.778,3.704), vals = NULL)
## load custom bathymetry raster
bathy_hiRes <- raster::raster(here('data','grids',"bathy_clipped"))
# aggregate and project bathymetry to survey grid cells, the absolute minimum resolution of the prediction grid
bathy_raster <- raster::projectRaster(bathy_hiRes, predict_raster, crs = CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"), method="bilinear")
# load Cowcod Conservation Areas, not included in trawl survey, and reproject
CCA = rgdal::readOGR(here('data','grids','kv299cy7357.shp'))
CCA = sp::spTransform(CCA, sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
# mask CCA from bathymetry raster used for prediction
bathy_raster = suppressWarnings(raster::mask(bathy_raster, CCA, inverse = TRUE))
# create matrix of point data with coordinates and depth from raster
wc_grid <- as.data.frame(raster::rasterToPoints(bathy_raster)) # rough area of survey extent is 123497km^2, from 2.778*3.704 (cell res) * nrow(wc_grid) = 12002 
colnames(wc_grid) = c("X", "Y", "depth")

# scale covariates
# edit-- where are these from?
wc_grid$log_depth_scaled <- (log(wc_grid$depth * -1) - mean(log(haul$depth_hi_prec_m))) / sd(log(haul$depth_hi_prec_m))
wc_grid$log_depth_scaled2 <- wc_grid$log_depth_scaled ^ 2

readr::write_rds(x = wc_grid, file=here('data','grids',"wc_grid.rds")) # save prediction grid
