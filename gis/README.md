
# TEMPEST spatial analysis
Objective is to provide regional context to the TEMPEST DOC export rates by placing the treatment within a regional context of storm surge exposure (duratioin & frequency) 

## Data sources: GIS layers compiled by Jim Holmquist
- HUC8 watersheds
- Hurricane tracks
- NOAA Hurricane surge map
- C-CAP map: Use latest static cover. Upland soils (land cover classifications : cultivated, FW wetland, upland forest/shrub). Exclude upland, not estuarine wetlands. Similar soils to Tempest, with OC that will defloculate. 


## Analysis steps: 
- Get overlap of Hurricane surge and CCAP map (selected classes)
- Get intersection of watersheds and hurricane tracks (tropical storms); Stormtrack database has a diameter for each leg.  Apply buffer around track based on hurricane diameter. 
- Calculate time periods between hurricanes.

## Plots
- Map of mean hurricane frequency per coastal watershed; over SE USA (Atlantic & Gulf Coast) to show the number of watershed; & area, & frequency 
- Boxplot: X-axis: individual watersheds; Y-axis: hurricane severity
- Bell-curve of area weighted flooding frequency, with TEMPEST treatment shown as percentile in the distribution. To show what is the analog to the Tempest experiment

