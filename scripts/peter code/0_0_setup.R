## This script sets up the environment for all follow-on scripts
##
## 2023-06-10
## Peter Regier
## 
# ########### #
# ########### #

## Load packages
require(pacman)
p_load(tidyverse, 
       #rdrop2, 
       cowplot, 
       lubridate, 
       #ggtukey, # adding letters to boxplots
      # multcompView, # compact letter display for comparing boxplots
       plotly,
       parsedate, # parse_date()
       janitor, # clean_names()
       purrr, # map()
       furrr,  # future_map()
       tictoc, # time stuff
       sf, 
       hms,
       ggallin,
       ggpubr, #stat_compare_means()
       #metR, # geom_contour_fill
       rnaturalearth,
       ggspatial, # north arrow and compass
       ggsflabel, # add labels
       googledrive, 
       ggthemes) #theme_map()
       
## Set ggplot theme
theme_set(theme_bw())

## Set common tz to use across all datetimes to EST (not sure why, but it's + 
## instead of - to get EST)
common_tz = "Etc/GMT+5"

## Set timestamps to truncate permanent datasets to pre-event and post-event
## boundaries. Note that the time-frame was selected to keep as much data as 
## possible, while removing non-flood events (i.e. rainfall around 5/30 and line
## flushing on 6/12)
pre_event_start = as.POSIXct("2023-06-02 00:00", tz = common_tz)
post_event_end = as.POSIXct("2023-06-11 23:55", tz = common_tz)

## Tighter bounds based on Firesting data
event_start = as.POSIXct("2023-06-05 00:00:00", tz = common_tz)
event_end = as.POSIXct("2023-06-12 00:00:00", tz = common_tz)

## Set timestamps to mark the start of each flood event. Note that events started
## at 6am EDT, but all timezones are in EST
dump_start1 = as.POSIXct("2023-06-06 05:00", tz = common_tz)
dump_end1 = as.POSIXct("2023-06-06 15:00", tz = common_tz)
dump_start2 = as.POSIXct("2023-06-07 05:00", tz = common_tz)
dump_end2 = as.POSIXct("2023-06-07 15:00", tz = common_tz)

## Helper functions that add na.rm to stats functions
mean_ <- function(var){mean({{var}}, na.rm = T)}
median_ <- function(var){median({{var}}, na.rm = T)}

## Helper function to label pre-determined time-periods 
label_flood_periods <- function(data){ 
  data %>% 
    #mutate(datetime = force_tz(datetime_est, tz = common_tz)) %>% 
    mutate(period = case_when(datetime_est >= dump_start1 - hours(24) & datetime_est < dump_start1 ~ "0_preflood", 
                              datetime_est >= dump_start1 & datetime_est < dump_end1 ~ "1_flood1", 
                              datetime_est >= dump_end1 & datetime_est < dump_start2 ~ "2_interflood",
                              datetime_est >= dump_start2 & datetime_est < dump_end2 ~ "3_flood2", 
                              datetime_est >= dump_end2 & datetime_est < dump_end2 + hours(24) ~ "4_postflood", 
                              TRUE ~ NA)) %>% 
    #filter(!is.na(period)) %>% 
    mutate(period_relabel = case_when(period == "0_preflood" ~ "Pre-Flood", 
                                      period == "1_flood1" ~ "Flood #1",
                                      period == "2_interflood" ~ "Inter-Flood",
                                      period == "3_flood2" ~ "Flood #2",
                                      period == "4_postflood" ~ "Post-Flood"))  %>% 
    mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Inter-Flood", "Flood #2", "Post-Flood"))
}


## Dashed line for start of event
add_line <- function(){
  geom_vline(xintercept = c(dump_start1, dump_start2), linetype = "dashed", 
             #color = "gray25", 
             color = "blue", 
             width = 2)
}

## Add geom_rect (use annotate: geom_rect creates many, many overlapping boxes)
add_box <- function(){
  annotate("rect", xmin = dump_start1, xmax = end_gmt, 
           ymin = -Inf, ymax = Inf, 
           fill = "lightblue", alpha = 0.7)
}

