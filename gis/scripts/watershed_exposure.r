


# /----------------------------------------------------------------------------#
#/        SET WD                                                         -------
library(here); setwd(here()) # setwd to the location of the project
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
sf_use_s2(F)


# Projection mishmash:
# HUC in EPSG:4326
# IBTrACS 
# slosh_grid
# CCAP 


# /----------------------------------------------------------------------------#
#/  Make Atlantic Coastline BBox
bbox_coords <- c(-100, 25, -60, 50)
names(bbox_coords) = c("xmin","ymin","xmax","ymax")
bbp = st_as_sfc(st_bbox(bbox_coords))
bbp = sf::st_set_crs(bbp, st_crs(4326))


# /----------------------------------------------------------------------------#
#/  Read in HUC8 watershed polygons
huc8 <- 
  st_read('../data/HUC8/HUC8_AllTidalNwiAndNonTidalPlusFarmedBelowMHHWS_ObviousOutliersRemoved.shp') %>% 
  st_transform('EPSG:4326') %>% 
  dplyr::select(AREA, HUC250K_ID) %>% 
  st_filter(., bbp, predicate=st_intersects)


# /----------------------------------------------------------------------------#
#/  Read storm surge tracks
# Wave height for radii defined in SEARAD USA_SEARAD_NE USA_SEA_NE nmile 
# Radial extent of seas (as defined in SEAHGT) 
# extending from storm center to the Northeast.
IBTrACS <- 
  st_read('../data/IBTrACS/IBTrACS.ALL.list.v04r01.lines/IBTrACS.ALL.list.v04r01.lines.shp') %>% 
  filter(SEASON >= 2000, 
         BASIN == 'NA',
         !is.na(USA_SSHS),  
         USA_SSHS >= 1,
         TRACK_TYPE=='main') %>% 
  mutate(USA_R34_mean_nmile = mean(c(USA_R34_NE, USA_R34_SE, USA_R34_NW, USA_R34_SW), na.rm=T)) %>% 
  # Convert radius from nmile to arcdeg
  mutate(USA_R34_mean_deg = USA_R34_mean_nmile/60) %>% 
  dplyr::select(SID, SEASON, NUMBER, ISO_TIME, USA_SSHS, USA_R34_mean_nmile, USA_R34_mean_deg)


# Get the time period included 
tperiod = max(IBTrACS$SEASON) - min(IBTrACS$SEASON)



# /-----------------------------------------------------------------------------
#/ Apply buffer to storm track
IBTrACS_buf <- 
  IBTrACS %>%
  # st_transform(., crs('EPSG:4269')) %>% 
  mutate(geometry = st_buffer(geometry, dist = USA_R34_mean_deg))


# /-----------------------------------------------------------------------------
#/  Spatial join 

huc8_j <- 
  st_join(huc8, IBTrACS_buf, join = st_intersects) %>% 
  filter(!is.na(USA_SSHS)) %>%
  # Remove duplicates;  A single storm cannot affect a watershed more than once
  distinct(HUC250K_ID, SID, .keep_all = TRUE) %>% 
  group_by(HUC250K_ID) %>%
  summarize(n=n(),
            n_cat1 = sum(USA_SSHS == 1),
            n_cat2 = sum(USA_SSHS == 2),
            n_cat3 = sum(USA_SSHS == 3),
            n_cat4 = sum(USA_SSHS == 4),
            n_cat5 = sum(USA_SSHS == 5)) %>% 
  ungroup()



# Map the count of storm tracks
huc8_j_p <- 
  huc8_j %>% 
  pivot_longer(cols=n:n_cat5, names_to='storm_cat', values_to = 'count') %>% 
  mutate(freq_stormperyear=count/tperiod) %>% 
  mutate(storm_cat_long = ifelse(storm_cat=='n', 'Total', NA),
         storm_cat_long = ifelse(storm_cat=='n_cat1', 'Category 1', storm_cat_long),
         storm_cat_long = ifelse(storm_cat=='n_cat2', 'Category 2', storm_cat_long),
         storm_cat_long = ifelse(storm_cat=='n_cat3', 'Category 3', storm_cat_long),
         storm_cat_long = ifelse(storm_cat=='n_cat4', 'Category 4', storm_cat_long),
         storm_cat_long = ifelse(storm_cat=='n_cat5', 'Category 5', storm_cat_long))



# /------------------------------------------------------------
#/   GET SLOSH model grid
library(terra)
slosh_filename_list <- list.files(path = '../data/SLOSH/US_SLOSH_MOM_Inundation_v3', 
                                  pattern = "_HIGH\\.tif$", full.names = TRUE)
slosh_filename_list

# Read in raster 
slosh_grid <- rast(slosh_filename_list[5])

# Reproject Slosh grid to CCAP
slosh_grid_p <- project(slosh_grid, crs(ccap_lu))

# Aggregate SLOSH grid to 
slosh_grid[slosh_grid == 0] <- NA
slosh_grid[slosh_grid > 0 & slosh_grid <= 21] <- 1
slosh_grid[slosh_grid > 21] <- NA



# /----------------------------------------------------------------------------#
#/  Read in CCAP
ccap_lu <- 
  rast('../../data/ccap/conus_2021_ccap_landcover_20241217.tif')
# project(., crs('EPSG:4326'))

# Reproject the bbox to the projection of CCAP
bbp_5070 <- st_transform(bbp, crs(ccap_lu))

# Crop to bbox  
ccap_lu <- crop(ccap_lu, bbp_5070)

# ccap_lu[ccap_lu <= 5] <- NULL
# ccap_lu[ccap_lu > 5] <- 1



# /------------------------------------------------------------
#/   
# Loop through huc8
for (i in 1:length(huc8)){

  # Subset to single watershed
  # huc8_sub <- huc8[i,]  
  huc8_sub <- huc8[100,]
  
  # Reproject HUC8 to the SLOSH Grid
  huc8_sub_reproj <- st_transform(huc8_sub, crs(slosh_grid))

    
  # Loop through 1-5 categories
  for (j in 1:length(slosh_filename_list)){
  
    # Read in raster 
    slosh_grid <- rast(slosh_filename_list[5])
  
    # Check if BBOX of SLOSH intersects with watershed
    slosh_grid_bbox <- st_bbox(slosh_grid) %>% st_as_sfc()
    if (sf::st_contains(slosh_grid_bbox, huc8_sub_reproj, sparse = FALSE)) {

      
      # Clip SLOSH to watershed
      slosh_grid_crop <- crop(slosh_grid, huc8_sub_reproj)
  
      # Convert water depth values
      slosh_grid_crop <- catalyze(slosh_grid_crop)
      slosh_grid_crop[slosh_grid_crop <= 21] <- 1
      slosh_grid_crop[slosh_grid_crop > 21] <- 0
      
    
      # Clip CCAP
      # Mask SLOSH with CCAP 
      
        
      # Multiply water mask by the number of storm category
    
      # Sum all SLOSH inundation events
    }
  }

  

# /-----------------------------------------------------------------------------
#/   Map buffered storm tracks
storm_track_map <-
  ggplot() +
  geom_sf(data=countries_sf, size=0.2) +

  # geom_sf(data=  IBTrACS_buf, aes(fill=USA_SSHS), color='grey60', size=0.05, alpha=0.25) +
  geom_sf(data= IBTrACS_buf, aes(color=USA_SSHS), fill=NA, lwd=0.35) + # , alpha=0.25) +
  geom_sf(data= huc8, fill='grey20', lwd=0) +
  scale_color_distiller(palette='YlGnBu', direction=1) +
  scale_x_continuous(limits=c(-120, -30)) +
  scale_y_continuous(limits=c(20, 60)) +
  coord_sf(expand=FALSE) +
  map_theme() +
  
  guides(color = guide_colorbar(
    nbin=5, raster=F, barheight = .6, barwidth=10, reverse=F,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title = expression(paste("Tropical storm category on the\nSaffir-Simpson Hurricane Scale"))))


ggsave('../output/figures/storm_track_map.png',
       storm_track_map,
       width=180, height=110, dpi=500, units='mm')



# /-----------------------------------------------------------------------------
#/   Map storm frequency per  
huc8_freq_map <- 
  ggplot() +
  geom_sf(data=countries_sf, size=0.2) +
  geom_sf(data= huc8_j_p, aes(fill=freq_stormperyear), lwd=0.02, color='grey20') +
  scale_fill_distiller(palette='YlOrRd', direction=1) +
  # theme(legend.position = 'none') +
  scale_x_continuous(limits=c(-100, -60)) +
  scale_y_continuous(limits=c(23, 50)) +
  coord_sf(expand=FALSE) +
  map_theme() +
  facet_wrap(.~storm_cat_long) +

  guides(fill = guide_colorbar(
    nbin=15, raster=F, barheight = .6, barwidth=10, reverse=F,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title = expression(paste("Storm frequency \nper HUC8 watershed\n(number of storm/year)"))))
    
  

ggsave('../output/figures/huc8_freq_map.png',
       huc8_freq_map,
       width=180, height=110, dpi=500, units='mm')




# /-----------------------------------------------------------------------------
#/   Barplot of watershed # exposed by each storm category
huc8_freq_barplot <- 
  ggplot() +
  geom_histogram(data= huc8_j_p, bins=20, 
                 aes(x=freq_stormperyear, fill=storm_cat_long), color='grey30', lwd=0.1) + # , fill=freq_stormperyear
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  scale_fill_brewer(palette='YlGnBu', direction=1) +
  ylab('Number of HUC8 watersheds') +
  xlab('Storm frequency (storm / year)') +
  line_plot_theme() +
  facet_wrap(.~storm_cat_long, scales='free_y', ncol=1) +
  theme(legend.position = 'none')


ggsave('../output/figures/huc8_freq_barplot_v2.png',
       huc8_freq_barplot,
       width=70, height=180, dpi=500, units='mm')


  
# scale_y_continuous(limits=c(23, 50)) +
# coord_sf(expand=FALSE) +


