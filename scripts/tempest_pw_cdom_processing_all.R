## This script imports corrected absorbance and fluorescence indicies from the Aqualog 
## at PNNL MCRL and exports clean, Level 0B QC'ed data. 
## Corrected Data are read in from GitHub, and are processed in matlabs. Scripts are also on github 
## 
## Created: 2022-11-18 by AMP
## Updated for system level analysis 11/13/23 by AMP

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive) # drive_ functions

#double check your wd. should be ../tempest-system-level-analysis
#if not you need to do new relative file pathing

getwd()

## Set Github filepath for CDOM data files

directory = "./data_do_not_commit/"

sample_key <- readRDS("~/GitHub/tempest-system-level-analysis/data/for processing/TMP_Event_June2022_META_PW_SOURCE_DateTime.rds")

sample_key_merging <- sample_key %>%
  mutate(date = stringr::str_remove_all(Date, "-")) %>%
  select(Timepoint,date) %>%
  distinct()

#this is NOT the run date date range, but rather sampling. 

sampling_date_start = lubridate::as_date("2022-06-13") 
sampling_date_end = lubridate::as_date("2022-07-22") 


# 2. Functions -----------------------------------------------------------------

## Create a function to read in data
read_eems <- function(data){
  #  read in data
  read.csv(file = data) %>% 
    rename(sample_id = Sample_ID,
           sample_description = Sample_Description) 
  #%>%
   # select(-sample_description)
  }

read_ids <- function(readme){
  # read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_id = Sample_ID,
           sample_description = Sample_Description) %>% 
    select(sample_name, sample_id, Action)
}
#Diluted samples have already been accounted for in the matlab script, so these corrections have been already applied.

# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files_eems <- list.files(path = directory, pattern = "SpectralIndices", full.names = TRUE) 
ReadMes <- list.files(path = directory, pattern = "key", full.names = TRUE) 

## Read in data, filter to TMP samples, and add sample name, add readme actions
eems <- files_eems %>% 
  map_df(read_eems) %>% 
  filter(grepl("TMP", sample_id)) %>% # filter to TMP samples only
  filter(!grepl("Ionic_Strength", sample_description)) %>% #filter out the ionic strenght stuff
  select(-sample_description) %>%
  mutate(sample_id = str_trim(sample_id, side = c("both", "left", "right"))) %>% #Get rid of those stupid white spaces!!!!!!!
  select(-SUVA254) %>% #don't have proper SUVA calculations done in Matlab, so filter this out...
  mutate(across(everything(),  ~ case_when(.x >=0 ~ .x))) %>% #turn negatives into NAs
  bind_rows()


key <- ReadMes %>%
  map_df(read_ids)

# 4. Merge data & metadata ---------------------------------------------------------------

eems_all = full_join(eems, key, by = "sample_id") %>%
  dplyr::filter(is.na(Action)) %>%
  select(-Action, -sample_id) 

eems_all_meta =  eems %>% 
  rename(sample_name = sample_id) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "PreW", "T0")) %>%
  dplyr::mutate(Event = stringr::str_extract(sample_name, "TMP"),
       Plot = stringr::str_extract(sample_name, 'FW|SW|C|ESTUARY'), 
       Grid = stringr::str_extract(sample_name, "B4|C3|C6|D5|E3|F4|F6|H3|H6|I5|SOURCE|BARGE|POOL|WELL"),
       Timepoint = stringr::str_extract(sample_name,"T[0-9]|HR[0-9]"),
       Timepoint = case_when(Timepoint == "HR8" ~ "HR7", #change the estuary HR8 to HR7
                             TRUE ~ Timepoint)) %>%
  left_join(sample_key_merging, by = "Timepoint") %>%
  mutate(date= case_when(is.na(date) ~ stringr::str_extract(sample_name, "[0-9]{8}"),
                         TRUE ~ date )) %>%
  mutate(date = lubridate::as_date(date, format = "%Y%m%d"))

PW_eems_all_meta <- eems_all_meta %>%
  filter(Grid != "SOURCE") %>%
  filter(Grid != "WELL") %>%
  filter(Grid != "BARGE") %>%
  filter(Grid == "POOL")





write_csv(PW_eems_all_meta, "./data/TMP_PW_EEMS_L1_POOL.csv")


