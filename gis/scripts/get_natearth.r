sf_use_s2(F)

library(sp)
library(sf)
library(raster)
library(terra)

# /-----------------------------------------------------------------------------
#/    SF !!! -- get polygon of continent outline                                     -----

natearth_dir <- "../../data/nat_earth"

# Set common extent lat-long bounds
com_ext <- extent(-180, 180,  -56, 85)

# Boundary box 
bbox_robin_sf <- st_read(natearth_dir, "ne_110m_wgs84_bounding_box") %>% 
  st_crop(com_ext) %>% 
  st_transform(CRS("+proj=robin")) 


# Coastline
library(rworldmap)
data(coastsCoarse)
coastsCoarse_robin_sf <- st_as_sf(coastsCoarse) %>% 
  st_crop(com_ext) %>% 
  st_transform(CRS("+proj=robin")) 


# Countries
countries_sf <- st_read(natearth_dir, "ne_110m_admin_0_countries")

# Countries in Robinson projection
countries_robin_sf <- st_read(natearth_dir, "ne_110m_admin_0_countries") %>% 
  st_crop(com_ext) %>% 
  st_transform(CRS("+proj=robin")) 


