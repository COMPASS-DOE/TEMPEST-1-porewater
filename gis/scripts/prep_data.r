

#  Prep inputs:
# - reclassify grids
# - Reproject grids 

# /-----------------------------------------------------------
#/  PREP in CCAP
ccap_lu <- 
  rast('../../data/ccap/conus_2021_ccap_landcover_20241217.tif')
# project(., crs('EPSG:4326'))

bbp_5070 <- st_transform(bbp, crs(ccap_lu))

# Crop to bbox  
ccap_lu <- crop(ccap_lu, bbp_5070)

ccap_lu[ccap_lu <= 5] <- NULL
ccap_lu[ccap_lu > 5] <- 1


# Regrid to CCAP resolution?


# SAVE TO FILE






# /------------------------------------------------------------
#/   PREP SLOSH model grid
library(terra)
slosh_filename_list <- list.files(path = '../data/SLOSH/US_SLOSH_MOM_Inundation_v3', 
                                  pattern = "_HIGH\\.tif$", full.names = TRUE)
slosh_filename_list

# Read in raster 
slosh_grid <- rast(slosh_filename_list[5])


# Reproject
slosh_grid <- project(slosh_grid, crs(ccap_lu))
