## This script imports analyte data for June 2022 and organizes it
## into categories (pre-event, during event, post-event). Averages for each time
## "bin" are calculated, and the min and max disturbance value during the event
## are also calculated.
##
## Data are read in from the GitHub "data" folder.
##
## Created: 2023-02 by Julia McElhinny for TEMPEST

# 1. Load Packages -------------------------------------------------------------

require(pacman)
pacman::p_load(dplyr, tidyr, ggplot2, readr, broom,
               janitor, lubridate)

# 2. Define Functions ----------------------------------------------------------

# Function to normalize datasets by calculating a zscore based on the standard 
# deviation of the Control plot for each analyte
# df is a data frame
# vars is a vector of one or more variables
# plot_col_name is a character vector
# scale_col_name is a character vector

zscore_standard = function(df, vars, scale_col_name){
  df %>%
    group_by({{scale_col_name}}) %>%
    mutate(across(vars,
                  list(zscore = ~(.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
                  .names = "{.fn}_{.col}"))
  
  
}

# write function to bin the data into pre, during, and after the flooding event
# "data" is the cleaned dataset from above, "datetime_name" is the name of the column with the datetime information
# "event start" and "event end" are the time points decided for the June 2022 event
# "2022-06-22 5:30:00 EST" and "2022-06-22 14:30:00 EST"
divide_data <- function(data, datetime_name, eventA, eventB = NULL, eventB_name = NULL) {

  data %>% ungroup() %>% dplyr::select({{datetime_name}}) -> datetime_column

  # If there is no second event...
  if(is.null(eventB)){

    # ...and if the datetime_name column is in YYYY-MM-DD (date) format...
    if(is.Date(datetime_column[[1,1]])) {
      
      as_date(eventA[1]) -> event
      
      data %>%
        mutate({{datetime_name}} := as_date({{datetime_name}}),
               timedate_bin = case_when(
                 {{datetime_name}} < event ~ "Pre",
                 {{datetime_name}} == event ~ "Mid",
                 {{datetime_name}} > event ~ "Post"
               ))
      
    } else {
      
      # ...otherwise treat is as a YYYY-MM-DD HH:MM:SS (timestamp)...
      data %>%
        mutate({{datetime_name}} := as.POSIXct({{datetime_name}}),
               timedate_bin = case_when(
                 {{datetime_name}} < as.POSIXct(eventA[1], tz = "EST") ~ "Pre",
                 {{datetime_name}} >= as.POSIXct(eventA[1], tz = "EST") & {{datetime_name}} <= as.POSIXct(eventA[2], tz = "EST") ~ "Mid",
                 {{datetime_name}} > as.POSIXct(eventA[2], tz = "EST") ~ "Post"
               ))
    }
    
  } else {
    # If there IS a second event...
    
    # ...and if the datetime_name column is in YYYY-MM-DD (date) format...
    #note for this to work for multiple events on discrete data, the events cannot
    #be on the same day
    if(is.Date(datetime_column[[1,1]])) {
      
      
      as_date(eventA[1]) -> eventA
      as_date(eventB[1]) -> eventB
      
      if(eventA == eventB) {
        stop("this function doesn't work with events on the same day")
      }
      
      data %>%
        mutate({{datetime_name}} := as_date({{datetime_name}}),
               timedate_bin = case_when(
                 {{datetime_name}} < eventA ~ "Pre",
                 {{datetime_name}} >= eventA & {{datetime_name}} <= eventA ~ "Mid",
                 {{datetime_name}} >= eventB & {{datetime_name}} <= eventB ~ eventB_name,
                 {{datetime_name}} > eventB ~ "Post",
                 TRUE ~ "Btwn Events"
               ))
      
    } else {
      
      # ...otherwise treat is as a YYYY-MM-DD HH:MM:SS (timestamp)
      data %>%
        mutate({{datetime_name}} := as.POSIXct({{datetime_name}}),
               timedate_bin = case_when(
                 {{datetime_name}} < as.POSIXct(eventA[1], tz = "EST") ~ "Pre",
                 {{datetime_name}} >= as.POSIXct(eventA[1], tz = "EST") & {{datetime_name}} <= as.POSIXct(eventA[2], tz = "EST") ~ "Mid",
                 {{datetime_name}} >= as.POSIXct(eventB[1], tz = "EST") & {{datetime_name}} <= as.POSIXct(eventB[2], tz = "EST") ~ eventB_name,
                 {{datetime_name}} > as.POSIXct(eventB[2], tz = "EST") ~ "Post",
                 TRUE ~ "Btwn Events"
               ))
    }
    
  }
  
}

# write function to do steps contained in original calc_dist_metrics function across multiple columns
# written for data streams that are continuous and/or have datetime labels
calc_dist_metrics <- function(data, datetime_name, event, analyte_col_name, scale_col_name) {
  
  # pull out name of the data frame being used
  data_name <- deparse(substitute(data))
  
  # bin and group the desired data frame
  grouped_binned_data <- divide_data(data, {{datetime_name}}, event) %>%
    group_by(timedate_bin, plot, {{scale_col_name}})

  # create empty list to populate with summary calculations
  sum_calcs_list <- list()
  
  # loop through calculations for each desired variable in the data frame
  for (a in seq_along(analyte_col_name)) {
    
    # pull out the analyte being worked on
    analyte <- analyte_col_name[a]
    
    sum_calcs <- grouped_binned_data %>%
      summarize(avg = mean(eval(parse(text = {{analyte_col_name}}[a])), na.rm = TRUE),
                min = min(eval(parse(text = {{analyte_col_name}}[a])), na.rm = TRUE),
                max = max(eval(parse(text = {{analyte_col_name}}[a])), na.rm = TRUE),
                time_max = {{datetime_name}}[which.max(eval(parse(text = {{analyte_col_name}}[a])))],
                sd = sd(eval(parse(text = {{analyte_col_name}}[a])), na.rm = TRUE),
                n = n()) %>%
      mutate(analyte_name = analyte)
    
    
    # assign sum_calcs to corresponding component of the list
    sum_calcs_list[[analyte]] <- sum_calcs
    
  }
  
  # bind rows of the list
  bind_sum <- bind_rows(sum_calcs_list)

  # edits to final data frames and resistance/resilience calculations
  bind_sum %>%
    pivot_wider(names_from = "timedate_bin", values_from = c("avg", "sd", "n", "min", "max", "time_max")) %>% # make wide dataframe with one row per plot per analyte
    select(-c(min_Pre, min_Post, max_Pre, max_Post)) %>% # remove min and max for post and pre timepoints
    # pull the value of the highest perturbation from pre-disturbance levels
    # use max value during the event if data is trending up even slightly
    # use min value during the event is data is trending down even slightly
    # use average value for calcs if the data is remaining constant
    mutate(max_effect_Mid = case_when(
      avg_Mid > avg_Pre ~ max_Mid,
      avg_Mid < avg_Pre ~ min_Mid,
      avg_Mid == avg_Pre ~ avg_Mid
    )) %>%
    # remove max_Mid and min_Mid
    select(-max_Mid, -min_Mid) %>%
    rename(Vpre = avg_Pre, Vpre_n = n_Pre, 
           Vpre_sd = sd_Pre, Vdist = max_effect_Mid, 
           Vdist_n = n_Mid, Vdist_sd = sd_Mid, 
           Vdist_avg = avg_Mid, Vpost = avg_Post, 
           Vpost_n = n_Post, Vpost_sd = sd_Post) %>%
    mutate(magnitude_change = Vdist - Vpre)
  
}

rate_recovery <- function(data, metrics, plot_col_name, vars) {

  data %>% 
    ungroup() %>% 
    select(plot, vars) %>% 
    dplyr::filter(plot == "Control") %>% 
    summarise(across(vars,
                     list(sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{.fn}_{.col}")) %>% 
    pivot_longer(everything(), names_to = "analyte_name", values_to = "control_sd") %>% 
    mutate(analyte_name = gsub("sd_", "", analyte_name),
           control_sd_3 = 3 * control_sd) -> control_sd
  
  metrics %>% 
    ungroup() %>% 
    filter(analyte_name %in% vars) %>% 
    dplyr::select(plot, analyte_name, Vpre) %>% 
    left_join(control_sd, by = "analyte_name") -> recov_metrics

  for(i in 1:length(vars)) {
    data %>% 
      ggplot(aes(x = force_tz(timestamp, tzone = "EST"), y = !!sym(vars[i]))) +
      annotate("rect", xmin=EVENT_START, xmax=EVENT_STOP, 
               ymin= -Inf, ymax=Inf, alpha=0.6, fill="lightblue") +
      geom_line() +
      facet_wrap(~plot) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Timestamp", y = paste(vars[i])) +
      theme(legend.position="bottom") ->> g
  }
  
}


