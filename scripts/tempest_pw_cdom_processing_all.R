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

directory = "./data_do_not_commit/cdom"

sample_key <- readRDS("~/GitHub/tempest-system-level-analysis/data/for processing/TMP_Event_June2022_META_PW_SOURCE_DateTime.rds")

sample_key_merging <- sample_key %>%
  mutate(date = stringr::str_remove_all(Date, "-")) %>%
  select(Timepoint,date) %>%
  distinct() 

non_event_sample_key <- readxl::read_excel("./data_do_not_commit/porewaterinventory.xlsx", skip=3, sheet="Individual") %>%
  select(Sample_ID, Evacuation_date_YYYMMDD, Collection_Date_YYYYMMDD, Collection_End_Time_24hrs, EST_EDT) %>%
  filter(str_detect(Sample_ID, "DOC")) %>%
  rename(sample_name = Sample_ID,
         collection_date = Collection_Date_YYYYMMDD,
         time = Collection_End_Time_24hrs,
         tz = EST_EDT) %>%
  select(sample_name, collection_date, time, tz)

estuary_key1 = tibble(Plot = "ESTUARY",
                      Timepoint = "HR4", 
                      date = "20220622", 
                      time= "110000")

estuary_key2 = tibble(Plot = "ESTUARY",
                      Timepoint = "HR7", 
                      date = "20220622", 
                      time= "150000")

sample_key_merging <- sample_key %>%
  mutate(date = stringr::str_remove_all(Date, "-")) %>%
  rename(time = Start_time) %>%
  mutate(time = str_replace_all(time, ":", "")) %>%
  select(Plot,Timepoint, date, time) %>%
  bind_rows(estuary_key1,estuary_key2) %>%
  mutate(time = str_replace(time, "^[0-9]{5}$", function(x) paste0("0",x)))

non_event_sample_key_merging <- non_event_sample_key %>%
  mutate(Plot = stringr::str_extract(sample_name, 'FW|SW|C|ESTUARY'), 
         Grid = stringr::str_extract(sample_name, "B4|C3|C6|D5|E3|F4|F6|H3|H6|I5|SOURCE|ESTUARY|POOL|WELL"),
         date = stringr::str_extract(sample_name, "[0-9]{8}"),
         time = str_replace(time, "^[0-9]{3}$", function(x) paste0("0", x, "00")),
         time = str_replace(time, "^[0-9]{4}$", function(x) paste0(x, "00"))
  ) %>%
  select(Plot, Grid, date, time) 

#this is NOT the run date date range, but rather sampling. 

endstudydate = lubridate::as_date("2024-01-31")
startstudydate = lubridate::as_date("2022-05-01")

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
files_eems

files_abs <- list.files(path = directory, pattern = "RSU", full.names = TRUE) 
files_abs

#ReadMes <- list.files(path = directory, pattern = "key", full.names = TRUE) 

## Read in data, filter to TMP samples, and add sample name, add readme actions
eems <- files_eems %>% 
  map_df(read_eems) %>% 
  filter(grepl("TMP", sample_id)) %>% # filter to TMP samples only
  filter(!grepl("Ionic_Strength", sample_description)) %>% #filter out the ionic strength stuff
  select(-sample_description) %>%
  mutate(sample_id = str_trim(sample_id, side = c("both", "left", "right"))) %>% #Get rid of those stupid white spaces!!!!!!!
  select(-SUVA254) %>% #don't have proper SUVA calculations done in Matlab, so filter this out...
  mutate(across(everything(),  ~ case_when(.x >=0 ~ .x))) %>% #turn negatives into NAs
 select(-Sample_shorthand) %>%
   bind_rows()

abs <- files_abs %>% 
  map_df(read_eems) %>% 
  filter(grepl("TMP", sample_id)) %>% # filter to TMP samples only
  filter(!grepl("Ionic_Strength", sample_description)) %>% #filter out the ionic strength stuff
  select(-sample_description) %>%
  mutate(sample_id = str_trim(sample_id, side = c("both", "left", "right"))) %>% #Get rid of those stupid white spaces!!!!!!!
  mutate(across(everything(),  ~ case_when(.x >=0 ~ .x))) %>% #turn negatives into NAs
  bind_rows()



# 4. Merge data & metadata ---------------------------------------------------------------

# eems_all = full_join(eems, key, by = "sample_id") %>%
#   dplyr::filter(is.na(Action)) %>%
#   select(-Action, -sample_id) 

eems_all_meta =  eems %>% 
  left_join(abs, by= "sample_id") %>%
  rename(sample_name = sample_id) %>%
  mutate(sample_name = stringr::str_replace(sample_name,"POOLED","POOL")) %>%
  mutate(sample_name = stringr::str_replace(sample_name,"Pooled","POOL")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "PreW", "T0")) %>%
  mutate(sample_name = stringr::str_remove(sample_name,"_CDOM")) %>%
  dplyr::mutate(Event = stringr::str_extract(sample_name, "TMP"),
       Plot = stringr::str_extract(sample_name, 'FW|SW|C|ESTUARY'), 
       Grid = stringr::str_extract(sample_name, "B4|C3|C6|D5|E3|F4|F6|H3|H6|I5|SOURCE|BARGE|POOL|WELL"),
       Timepoint = stringr::str_extract(sample_name,"T[0-9]|HR[0-9]"),
       Timepoint = case_when(Timepoint == "HR8" ~ "HR7", #change the estuary HR8 to HR7
                             TRUE ~ Timepoint)) 
  



#Identify if any duplicates were run, this should return an empty data frame if not:#

duplicates <- eems_all_meta %>% subset(duplicated(sample_name))

View(duplicates)

# there are some replicates: 
reps <- eems_all_meta %>%
  group_by(sample_name) %>%
  filter(n() > 1) 

View(reps)

reps_names <- reps %>%
  select(sample_name) %>%
  unique() 

eems_all_meta_no_reps <- eems_all_meta %>%
  filter(!sample_name %in% reps_names$sample_name)

#need to remove a rep if the following conditions are met:
# 1) Flag says "high blank" &
# 2) Values are > 25% apart 
# If second condition is met but the first is not met, need to flag with "Inconsistent reps"
# If they are close in value, regardless of condition for 1), can be confident that they look good. 

reps_clean <- reps %>%
  group_by(sample_name) %>%
  mutate(across(S275_295:FDOM_RSU, 
                list(max = max, min = min, percerr = ~ (max(.) - min(.)) / max(.), 
                     Keep = ~ case_when((max(.) - min(.)) / max(.) < .25 ~ TRUE, TRUE ~ NA)), 
                .names = "{.col}_{.fn}")) %>% # these are all within 25% so merge
  select(S275_295:Timepoint) %>%
  summarise(across(S275_295:FDOM_RSU, 
                   ~ mean(., na.rm = TRUE)),
            across(everything(), first, .names = "{.col}")) 

# 7. Clean data ----------------------------------------------------------------

#Need to merge those:
eems_all_meta_dups_merged <- eems_all_meta_no_reps %>% 
  bind_rows(reps_clean) %>%
  left_join(sample_key_merging, by = c("Plot","Timepoint")) %>%
  mutate(date= case_when(is.na(date) ~ stringr::str_extract(sample_name, "[0-9]{8}"),
                         TRUE ~ date )) %>%
  left_join(non_event_sample_key_merging, by= c("Plot","Grid","date"), suffix = c("", ".fill")) %>%
  mutate(time = coalesce(time, time.fill)) %>%
  select(-time.fill) %>%
  mutate(time = case_when(is.na(time) ~ stringr::str_extract(sample_name, "(?<=[0-9]{8}_)\\d{4}"),
                          TRUE ~ time),
         time = str_replace(time, '\\d+', function(m) str_pad(m, 6, pad = '0', side = ("right")))
  ) %>%
  mutate(time = case_when(is.na(time) ~ "115900", 
                          TRUE ~ time)) %>% #if no time recorded in the metadata sheet, fill in with noon
  mutate(Timepoint = case_when(is.na(Timepoint) & date == "2022-07-18" ~ "T5",
                               is.na(Timepoint) & date == "2022-07-21" ~ "T5",
                               is.na(Timepoint) & date == "2022-06-15" ~ "T0",
                               is.na(Timepoint) & date == "2022-06-13" ~ "T0",
                               TRUE ~ Timepoint)) %>%
  mutate(date = lubridate::as_date(date, format = "%Y%m%d"),
         time= strptime(time, format ="%H%M%S"),
         time = strftime(time, "%H:%M:%S"))  %>%
  mutate(Group=case_when(lubridate::month(date) != 6 ~ as.character(paste(lubridate::month(date), lubridate::year(date), sep = "-")),
                         lubridate::month(date) == 6 ~ as.character(date)))

#Only porewater POOLED, all dates:
PW_eems_all_pooled_only <- eems_all_meta_dups_merged %>%
  filter(date < endstudydate & date > startstudydate) %>%
  filter(Grid != "SOURCE") %>%
  filter(Grid != "WELL") %>%
  filter(Grid != "BARGE") %>%
  filter(Grid == "POOL")

#Only porewater NOT pooled, all dates:
PW_eems_all <- eems_all_meta_dups_merged %>%
  filter(date < endstudydate & date > startstudydate) %>%
  filter(Grid != "SOURCE") %>%
  filter(Grid != "WELL") %>%
  filter(Grid != "BARGE") %>%
  filter(Grid != "POOL")

#Only sourcewater, all dates:
source_eems_all <- eems_all_meta_dups_merged %>%
  filter(date < endstudydate & date > startstudydate) %>%
  filter(grepl("SOURCE|ESTUARY|BARGE", sample_name)) 

saveRDS(source_eems_all, "../TEMPEST-1-porewater/processed data/TMP_SOURCE_CDOM_L1_TMP1TMP2.rds")

saveRDS(PW_eems_all_pooled_only, "../TEMPEST-1-porewater/processed data/TMP_PW_POOLED_CDOM_L1_May2022-Dec2023.rds")

saveRDS(PW_eems_all, "../TEMPEST-1-porewater/processed data/TMP_PW_grids_CDOM_L1_May2022-Dec2023.rds")
