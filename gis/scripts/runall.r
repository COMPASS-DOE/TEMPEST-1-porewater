# Run all script for spatial analysis of TEMPEST DOC export
# 



# /----------------------------------------------------------------------------#
#/        SET WD                                                         -------
library(here); setwd(here()) # setwd to the location of the project

library(tidyverse)
library(dplyr)
library(sf)


source('themes.r')

# Unzip data to memory =========================================================

# Coastal watersheds HUC8 ------------------------------------------------------

# NOAA Hurricane tracks  -------------------------------------------------------


# SLOSH - NOAA Hurricane surge map
# https://www.nhc.noaa.gov/nationalsurge/


# C-CAP map --------------------------------------------------------------------
# Use latest static cover. Upland soils (land cover classifications: 
# cultivated, FW wetland, upland forest/shrub). Exclude upland, not estuarine wetlands. 
# Similar soils to Tempest, with OC that will defloculate.



# Processing steps =============================================================

# Get overlap of Hurricane surge and CCAP map (selected classes) -----------------

# Get intersection of watersheds and hurricane tracks (tropical storms); 
# Stormtrack database has a diameter for each leg. 
# Apply buffer around track based on hurricane diameter.

# Calculate time periods between hurricanes.


# Plots ====================================================================

# Map of mean hurricane frequency per coastal watershed; 
# over SE USA (Atlantic & Gulf Coast) 
# to show the number of watershed; & area, & frequency

# Boxplot: X-axis: individual watersheds; Y-axis: hurricane severity

# Bell-curve of area weighted flooding frequency, with TEMPEST treatment shown as percentile in the distribution. To show what is the analog to the Tempest experiment