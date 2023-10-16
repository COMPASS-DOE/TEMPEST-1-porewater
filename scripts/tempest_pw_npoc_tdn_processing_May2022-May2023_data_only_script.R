## This script imports raw data for NPOC and TDN measured using a Shimadzu TOC-L
## at PNNL MCRL and exports clean, Level 1 QC'ed data. 
## Raw Data are read in from GitHub
## 
## Created: 2022-01-15 by Peter Regier for EXCHANGE
## Updated: 2022-06-26 by Allison Myers-Pigg for TEMPEST
## 
##
# #############
# #############

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive) # drive_ functions

#double check your wd. should be ../tempest-system-level-analysis
#if not you need to do new relative file pathing
#this is where the TEMPEST DOC data is currently stored. Subject to reorg. 

getwd()

## Set Github filepath for NPOC raw data files:

directory = "../tempest-system-level-analysis/data/raw/DOC"

# 2. Functions -----------------------------------------------------------------

## Create a function to read in data
read_data <- function(data){
  # First, scrape date from filename
  rundate <- str_extract(data, "[0-9]{8}")
  # Second, read in data
  read_delim(file = data, skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tdn_raw = `Result(TN)`,
           run_datetime = `Date / Time`) %>% 
    select(sample_name, npoc_raw, tdn_raw,run_datetime) %>% 
    mutate(rundate = rundate)
}

read_mes <- function(readme){
  # First, scrape date from filename
  rundate <- str_extract(readme, "[0-9]{8}")
  # Second, read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_name = `Sample Name`,
           sample_vol = `Sample wt`,
           total_vol = `Total vol:`) %>% 
    select(sample_name, Action, sample_vol, total_vol) %>% 
    mutate(rundate = rundate)
}

# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- list.files(path = directory, pattern = "Summary", full.names = TRUE) 
ReadMes <- list.files(path = directory, pattern = "Readme", full.names = TRUE) 

npoc_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(grepl("202208|202209|202210|202211|202212|202301|202302|202303|202304|202305|202306", rundate)) %>% # filter to just run dates you need 
  bind_rows() 

blanks_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("^Blank", sample_name)) %>% # filter to blanks only
  filter(grepl("202208|202209|202210|202211|202212|202301|202302|202303|202304|202305|202306", rundate)) %>% # filter to just run dates you need
  bind_rows() 

readmes_dilution_action <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(grepl("ilution correction", Action)) %>%
  filter(grepl("202208|202209|202210|202211|202212|202301|202302|202303|202304|202305|202306", rundate)) %>% # filter to just run dates you need
  bind_rows() 

readmes_all <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(grepl("202208|202209|202210|202211|202212|202301|202302|202303|202304|202305|202306", rundate)) %>% # filter to just run dates you need 
  mutate(Action = case_when(sample_name == "TMP_SW_F4_T3" ~ "Omit", # due to incorrect duplicate naming
                            TRUE ~ Action)) %>%
  bind_rows() 

curvepts <-files %>% 
  map_df(read_data) %>% 
  filter(grepl("STD_", sample_name)) %>% # filter to curves only
  filter(grepl("202208|202209|202210|202211|202212|202301|202302|202303|202304|202305|202306", rundate)) %>%
  filter(!grepl("10/25/2022 7:24:33 PM|9/22/2022 9:03:23 PM", run_datetime)) %>% # filter to just run dates you need 
  rename(standard_high_C = npoc_raw,
         standard_high_N = tdn_raw) %>%
  select(rundate,standard_high_C,standard_high_N) %>%
  pivot_longer(cols = c(standard_high_C,standard_high_N)) %>%
  na.omit() %>%
  group_by(rundate) %>%
  # COME BACK HERE
  distinct()%>%
  pivot_wider(names_from= name, values_from = value)%>%
  bind_rows() 


# 4. Calculate blanks and add to data ------------------------------------------

blanks <- blanks_raw %>% 
  filter(!run_datetime %in% NA) %>% 
  mutate(npoc_raw = ifelse(npoc_raw > 0, npoc_raw, NA)) %>%
  mutate(tdn_raw = ifelse(tdn_raw > 0, tdn_raw, NA)) %>%
  group_by(rundate) %>% 
  summarize(npoc_blank= round(mean(npoc_raw[!is.na(npoc_raw)]), 2),
            npoc_blank_SD= round(sd(npoc_raw[!is.na(npoc_raw)]), 2), #add SD columns
            tdn_blank= round(mean(tdn_raw[!is.na(tdn_raw)]), 2),
            tdn_blank_SD= round(sd(tdn_raw[!is.na(tdn_raw)]), 2)) %>% #add SD columns
  select(rundate, npoc_blank, npoc_blank_SD, tdn_blank, tdn_blank_SD)

View(blanks) # Check out the blank data 

# 5. Flag sketch data -----------------------------------------------------------

npoc_flagged <- npoc_raw %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  inner_join(blanks, by = "rundate") %>% 
  inner_join(curvepts, by= "rundate") %>%
  mutate(tdn_flag = case_when(tdn_raw > standard_high_N ~ "value above cal curve",
                              tdn_blank > 0.25*tdn_raw ~ "blank is ≥ 25% of sample value" # flagging if blank concentration is > 20% of the sample concentration 
                               ), 
         #most curves only to 50, those samples were not above it. making 100 for the August and September, which used 0-100
         npoc_flag = case_when(npoc_raw > standard_high_C ~ "value above cal curve",
                               npoc_blank > 0.25*npoc_raw ~ "blank is ≥ 25% of sample value" # flagging if blank concentration is > 20% of the sample concentration
                               )
         # npoc_raw = case_when(npoc_flag == "incorrect sample naming, cannot resolve" ~ NA,
         #                      TRUE ~ npoc_raw),
         # tdn_raw = case_when(tdn_flag == "incorrect sample naming, cannot resolve" ~ NA,
         #                     TRUE ~ tdn_raw)
  )

# 6. Dilution Corrections ------------------------------------------------------

dilutions = 
  readmes_dilution_action %>% 
  mutate(Dilution =  total_vol/sample_vol) %>% 
  dplyr::select(rundate, sample_name, Action, Dilution) %>% 
  force()

samples_to_dilution_corrected = 
  npoc_flagged %>%
  left_join(dilutions, by = c("sample_name", "rundate")) %>% 
  filter(grepl("ilution correction", Action)) %>%
  filter(!Action %in% "Omit") %>% 
  mutate(doc_mg_l= npoc_raw * Dilution, tdn_mg_l = tdn_raw * Dilution, # True concentration = diluted concentration * total vol / sample vol
         doc_mg_l = as.numeric(doc_mg_l), doc_mg_l = round(doc_mg_l, 2),
         tdn_mg_l= as.numeric(tdn_mg_l), tdn_mg_l= round(tdn_mg_l, 2)) %>%
  mutate(doc_mg_l = case_when(Dilution > 30 & npoc_flag == "blank is ≥ 25% of sample value" ~ NA,
                              TRUE ~ doc_mg_l), # removing values if high blanks and high dilution ratios, potentially large source of error. 
         npoc_flag = case_when(is.na(doc_mg_l) ~ "omitted for high dilution and blank values",
                               TRUE ~ npoc_flag),
         tdn_mg_l = case_when(Dilution > 30 & tdn_flag == "blank is ≥ 25% of sample value" ~ NA,
                              TRUE ~ tdn_mg_l),
         tdn_flag = case_when(is.na(tdn_mg_l) ~ "omitted for high dilution and blank values",
                              TRUE ~ tdn_flag)) # removing values if high blanks and high dilution ratios, potentially large source of error. 

all_samples_dilution_corrected =
  npoc_flagged %>%
  left_join(readmes_all, by = c("sample_name", "rundate")) %>% 
  mutate(doc_mg_l = npoc_raw, tdn_mg_l = tdn_raw) %>%
  filter(!grepl("ilution correction", Action)) %>% 
  filter(!Action %in% "Omit") %>%
  bind_rows(samples_to_dilution_corrected) %>%
  dplyr::select(sample_name, rundate, doc_mg_l, tdn_mg_l, npoc_flag, tdn_flag)%>%
  mutate(doc_mg_l = if_else(doc_mg_l < 0, "NA", as.character(doc_mg_l)),
         tdn_mg_l = if_else(tdn_mg_l < 0, "NA", as.character(tdn_mg_l)),
         doc_mg_l = as.numeric(doc_mg_l), doc_mg_l = round(doc_mg_l, 2),
         tdn_mg_l= as.numeric(tdn_mg_l), tdn_mg_l= round(tdn_mg_l, 2))

#Identify if any duplicates were run, this should return an empty data frame if not:#

duplicates <- all_samples_dilution_corrected %>% subset(duplicated(sample_name))

View(duplicates)


#MANUALLY Fix sample names and see if there were multiple of the same sample split for some reason run:

all_samples_dilution_corrected2 <- all_samples_dilution_corrected %>% 
  mutate(sample_name = stringr::str_replace(sample_name,"_A","")) %>%
  mutate(sample_name = stringr::str_replace(sample_name,"_B","")) %>%
  mutate(sample_name = stringr::str_replace(sample_name,"POOLED","POOL")) %>%
  mutate(sample_name = stringr::str_replace(sample_name,"_Subsample","")) 
  
reps <- all_samples_dilution_corrected2  %>%
  group_by(sample_name) %>%
  filter(n() > 1) 

View(reps)

reps_names <- reps %>%
  select(sample_name) %>%
  unique() 

samples_dilution_corrected2_no_reps <- all_samples_dilution_corrected2 %>%
  filter(!sample_name %in% reps_names$sample_name)

#need to remove a rep if the following conditions are met:
# 1) Flag says "high blank" &
# 2) Values are > 20% apart 
# If second condition is met but the first is not met, need to flag with "Inconsistent reps"
# If they are close in value, regardless of condition for 1), can be confident that they look good. 

reps_clean <- reps %>%
  group_by(sample_name) %>%
  mutate(doc_mg_l_max = max(doc_mg_l),
         doc_mg_l_min = min(doc_mg_l),
         doc_mg_l_percerr = (doc_mg_l_max - doc_mg_l_min) / doc_mg_l_max,
         Keep_doc = case_when(doc_mg_l_percerr < .25 ~ TRUE),
         tdn_mg_l_max = max(tdn_mg_l),
         tdn_mg_l_min = min(tdn_mg_l),
         tdn_mg_l_percerr = (tdn_mg_l_max - tdn_mg_l_min) / tdn_mg_l_max,
         Keep_tdn = case_when(tdn_mg_l_percerr < .25 ~ TRUE),
         doc_mg_l = case_when(Keep_doc == TRUE ~ doc_mg_l,
                              FALSE ~ NA),
         tdn_mg_l = case_when(Keep_tdn == TRUE ~ tdn_mg_l,
                              FALSE ~ NA)) %>%
  select(sample_name, doc_mg_l, tdn_mg_l, npoc_flag, tdn_flag) %>%
  summarise(doc_mg_l = mean(doc_mg_l, na.rm = TRUE),
            tdn_mg_l = mean(tdn_mg_l, na.rm = TRUE)) %>%
  mutate(npoc_flag = case_when(doc_mg_l == 'NaN' ~ "replicates greater than 25% different"),
         tdn_flag = case_when(tdn_mg_l == 'NaN' ~ "replicates greater than 25% different"),
         date = stringr::str_extract(sample_name, "[0-9]{8}"))

## 7. Clean data ----------------------------------------------------------------

#Need to merge those:
npoc_dups_merged <- samples_dilution_corrected2_no_reps %>% 
  bind_rows(reps_clean)

## Flagging data
npoc_flags <- npoc_dups_merged %>% 
  ## add flags 
  # Below blank 
  mutate(npoc_flag = case_when(doc_mg_l == 'NaN'& sample_name %in% reps_clean$sample_name ~ "replicates greater than 25% different",
                               sample_name %in% reps_clean$sample_name ~ "average value of multiple aliquots",
                               doc_mg_l == 'NaN' ~ "value below blank",
                               grepl("POOL", sample_name) ~ "pooled sample",
                               grepl("pool", sample_name) ~ "pooled sample",
                               grepl("value above cal curve",npoc_flag) ~ "value above cal curve",
                               TRUE ~ npoc_flag), 
         tdn_flag = case_when(tdn_mg_l == 'NaN'& sample_name %in% reps_clean$sample_name ~ "replicates greater than 25% different",
                              sample_name %in% reps_clean$sample_name ~ "average value of multiple aliquots",
                              tdn_mg_l == 'NaN' ~ "value below blank",
                              grepl("POOL", sample_name) ~ "pooled sample",
                              grepl("pool", sample_name) ~ "pooled sample",
                              grepl("value above cal curve",tdn_flag) ~ "value above cal curve",
                              TRUE ~ tdn_flag)) 


npoc_wflags_metadata <-  npoc_flags %>%
  mutate(sample_name = stringr::str_replace(sample_name,"pooled","POOL"),
         Event = stringr::str_extract(sample_name, "TMP"),
         Plot = stringr::str_extract(sample_name, 'FW|SW|C|ESTUARY'), 
         Grid = stringr::str_extract(sample_name, "B4|C3|C6|D5|E3|F4|F6|H3|H6|I5|SOURCE|BARGE|POOL|WELL"),
         Pool_Timepoint = stringr::str_extract(sample_name,"[0-9]{8}_\\d{8}|[0-9]{8}-\\d{8}"),
         sample_name = stringr::str_remove(sample_name,"(?<=[0-9]{8})_\\d{8}|(?<=[0-9]{8})-\\d{8}"),
         sample_name = stringr::str_remove(sample_name, "_FW\\d{2}|_SW\\d{2}"),
         doc_mg_l = case_when(doc_mg_l == "NaN" ~ NA,
                              TRUE ~ doc_mg_l),
         tdn_mg_l = case_when(tdn_mg_l == "NaN" ~ NA,
                              TRUE ~ tdn_mg_l)) %>%
  mutate(date= stringr::str_extract(sample_name, "[0-9]{8}"),
         time = stringr::str_extract(sample_name, "(?<=[0-9]{8}_)\\d{4}"),
         time = str_replace(time, '\\d+', function(m) str_pad(m, 6, pad = '0', side = ("right")))
         ) %>%
  mutate(date = lubridate::as_date(date, format = "%Y%m%d"),
         time= strptime(time, format ="%H%M%S"),
         time = strftime(time, "%H:%M:%S"))

endstudydate = lubridate::as_date("2023-05-31")

PW_npoc_wflags_metadata <- npoc_wflags_metadata %>%
  filter(date < endstudydate) %>%
  filter(Grid != "SOURCE") %>%
  filter(Grid != "WELL") %>%
  filter(Grid != "BARGE") 

# 8. Write data ----------------------------------------------------------------

#not sure the blank is >25% is staying to the end of this data frame 
write_csv(PW_npoc_wflags_metadata , "../TEMPEST-1-porewater/data/TMP_PW_NPOC_TDN_L1_Jul2023-May2023.csv")


