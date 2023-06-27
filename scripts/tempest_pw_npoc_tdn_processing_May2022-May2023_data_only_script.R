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

directory = "./data/raw/DOC"

# 2. Functions -----------------------------------------------------------------

## Create a function to read in data
read_data <- function(data){
  # First, scrape date from filename
  date <- str_extract(data, "[0-9]{8}")
  # Second, read in data
  read_delim(file = data, skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tdn_raw = `Result(TN)`,
           run_datetime = `Date / Time`) %>% 
    select(sample_name, npoc_raw, tdn_raw,run_datetime) %>% 
    mutate(date = date)
}

read_mes <- function(readme){
  # First, scrape date from filename
  date <- str_extract(readme, "[0-9]{8}")
  # Second, read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_name = `Sample Name`,
           sample_vol = `Sample wt`,
           total_vol = `Total vol:`) %>% 
    select(sample_name, Action, sample_vol, total_vol) %>% 
    mutate(date = date)
}
# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- list.files(path = directory, pattern = "Summary", full.names = TRUE) 
ReadMes <- list.files(path = directory, pattern = "Readme", full.names = TRUE) 

## Read in data, filter to TMP samples, and add sample name, add readme actions
npoc_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(!grepl("202306", date)) %>% #filter to 2022-May 2023 dates> note this only works when run before data uploaded in July 2023. Will need to modify if re-running later. 
  bind_rows() 

blanks_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("^Blank", sample_name)) %>% # filter to TMP samples only
  filter(!grepl("202306", date)) %>% #filter to 2022-May 2023 dates> note this only works when run before data uploaded in July 2023. Will need to modify if re-running later. 
  bind_rows() 

readmes_dilution_action <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(grepl("ilution correction", Action)) %>%
  filter(!grepl("202306", date)) %>% #filter to 2022-May 2023 dates> note this only works when run before data uploaded in July 2023. Will need to modify if re-running later. 
  bind_rows() 

readmes_all <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(!grepl("202306", date)) %>% #filter to 2022-May 2023 dates> note this only works when run before data uploaded in July 2023. Will need to modify if re-running later. 
  mutate(Action = case_when(sample_name == "TMP_SW_F4_T3" ~ "Omit", # due to incorrect duplicate naming
                            TRUE ~ Action)) %>%
  bind_rows() 

# 4. Calculate blanks and add to data ------------------------------------------

blanks <- blanks_raw %>% 
  filter(!run_datetime %in% NA) %>% 
  mutate(npoc_raw = ifelse(npoc_raw > 0, npoc_raw, NA)) %>%
  mutate(tdn_raw = ifelse(tdn_raw > 0, tdn_raw, NA)) %>%
  group_by(date) %>% 
  summarize(npoc_blank= round(mean(npoc_raw[!is.na(npoc_raw)]), 2),
            npoc_blank_SD= round(sd(npoc_raw[!is.na(npoc_raw)]), 2),#add SD columns
            tdn_blank= round(mean(tdn_raw[!is.na(tdn_raw)]), 2),
            tdn_blank_SD= round(sd(tdn_raw[!is.na(tdn_raw)]), 2)) %>% 
  #summarize(npoc_blank_raw = round(mean(npoc_raw[!is.na(npoc_raw)]), 2), 
  #         tdn_blank_raw = round(mean(tdn_raw[!is.na(tdn_raw)]), 2)) %>% 
  #mutate(npoc_blank = ifelse(npoc_blank_raw > lod_npoc, npoc_blank_raw, 0), 
  #       tdn_blank = ifelse(tdn_blank_raw > lod_tdn, tdn_blank_raw, 0)) %>% 
  select(date, npoc_blank, npoc_blank_SD, tdn_blank, tdn_blank_SD)

View(blanks) # Check out the blank data 

# 5. Add blanks data -----------------------------------------------------------

npoc_flagged <- npoc_raw %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  inner_join(blanks, by = "date") %>% 
  mutate(tdn_flag = case_when(tdn_raw > 3 ~ "value above cal curve",
                              tdn_blank > 0.2*tdn_raw ~ "high blank", # flagging if blank concentration is > 20% of the sample concentration 
                              sample_name == "TMP_SW_F4_T3" ~ "incorrect sample naming, cannot resolve"), 
         #most curves only to 50, those samples were not above it. making 100 for the August and September, which used 0-100
         npoc_flag = case_when(npoc_raw > 100 ~ "value above cal curve",
                               npoc_blank > 0.2*npoc_raw ~ "high blank", # flagging if blank concentration is > 20% of the sample concentration
                               sample_name == "TMP_SW_F4_T3" ~ "incorrect sample naming, cannot resolve"),
         npoc_raw = case_when(npoc_flag == "incorrect sample naming, cannot resolve" ~ NA,
         TRUE ~ npoc_raw),
         tdn_raw = case_when(tdn_flag == "incorrect sample naming, cannot resolve" ~ NA,
                              TRUE ~ tdn_raw)
  )

# 6. Dilution Corrections ------------------------------------------------------

dilutions = 
  readmes_dilution_action %>% 
  mutate(Dilution =  total_vol/sample_vol) %>% 
  dplyr::select(date, sample_name, Action, Dilution) %>% 
  force()

samples_dilution_corrected = 
  npoc_flagged %>%
  left_join(dilutions, by = c("sample_name", "date")) %>% 
  filter(grepl("ilution correction", Action)) %>%
  filter(!Action %in% "Omit") %>% 
  mutate(doc_mg_l= npoc_raw * Dilution, tdn_mg_l = tdn_raw * Dilution, # True concentration = diluted concentration * total vol / sample vol
         doc_mg_l = as.numeric(doc_mg_l), doc_mg_l = round(doc_mg_l, 2),
         tdn_mg_l= as.numeric(tdn_mg_l), tdn_mg_l= round(tdn_mg_l, 2)) %>%
  mutate(doc_mg_l = case_when(Dilution > 30 & npoc_flag == "high blank" ~ NA,
                              TRUE ~ doc_mg_l), # removing values if high blanks and high dilution ratios, potentially large source of error. 
         npoc_flag = case_when(is.na(doc_mg_l) ~ "omitted for high dilution and blank values",
                               TRUE ~ npoc_flag),
         tdn_mg_l = case_when(Dilution > 30 & tdn_flag == "high blank" ~ NA,
                              TRUE ~ tdn_mg_l),
         tdn_flag = case_when(is.na(tdn_mg_l) ~ "omitted for high dilution and blank values",
                               TRUE ~ tdn_flag)) # removing values if high blanks and high dilution ratios, potentially large source of error. 

all_samples_dilution_corrected =
  npoc_flagged %>%
  left_join(readmes_all, by = c("sample_name", "date")) %>% 
  mutate(doc_mg_l = npoc_raw, tdn_mg_l = tdn_raw) %>%
  filter(!grepl("ilution correction", Action)) %>% 
  filter(!Action %in% "Omit") %>%
  bind_rows(samples_dilution_corrected) %>%
  dplyr::select(sample_name, date, doc_mg_l, tdn_mg_l, npoc_flag, tdn_flag)%>%
  mutate(doc_mg_l = if_else(doc_mg_l < 0, "NA", as.character(doc_mg_l)),
         tdn_mg_l = if_else(tdn_mg_l < 0, "NA", as.character(tdn_mg_l)),
         doc_mg_l = as.numeric(doc_mg_l), doc_mg_l = round(doc_mg_l, 2),
         tdn_mg_l= as.numeric(tdn_mg_l), tdn_mg_l= round(tdn_mg_l, 2))

#Identify if duplicates were run#


duplicates <- all_samples_dilution_corrected %>% subset(duplicated(sample_name))

View(duplicates)


#MANUALLY Fix sample names and see if there were multiple of the same sample split for some reason run:

all_samples_dilution_corrected2 <- all_samples_dilution_corrected %>% 
  mutate(sample_name = stringr::str_replace(sample_name,"_DOC","")) %>%
  mutate(sample_name = stringr::str_replace(sample_name,"HR6","HR7")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_DILUTED4mLsmpl3mLwater", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_Diluted", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_1of1", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_1of2", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_2of2", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_1of3", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_2of3", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "_3of3", "")) %>%
  mutate(sample_name = stringr::str_replace(sample_name, "PreW", "T0")) %>% #need to double check name
  mutate(sample_name = stringr::str_replace(sample_name, "_EXTRA", "")) %>% #sample sent a different day run for DOC only
  mutate(sample_name = stringr::str_replace(sample_name, "TMP_C_POOL_T2", "TMP_C_H6_T2")) %>% # control pooled T2 is actually not a pooled sample - field metadata sheets have that sample coming from one grid: H6
  mutate(sample_name = stringr::str_replace(sample_name, "TMP_C_F4_20221128", "TMP_C_F6_20221128")) # nov/dec C F6 naming mix up with F4 in DOC run


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
         Keep_doc = case_when(doc_mg_l_percerr < .2 ~ TRUE),
         tdn_mg_l_max = max(tdn_mg_l),
         tdn_mg_l_min = min(tdn_mg_l),
         tdn_mg_l_percerr = (tdn_mg_l_max - tdn_mg_l_min) / tdn_mg_l_max,
         Keep_tdn = case_when(tdn_mg_l_percerr < .2 ~ TRUE),
         doc_mg_l = case_when(Keep_doc == TRUE ~ doc_mg_l,
                              FALSE ~ NA),
         tdn_mg_l = case_when(Keep_tdn == TRUE ~ tdn_mg_l,
                              FALSE ~ NA)) %>%
  select(sample_name, doc_mg_l, tdn_mg_l, npoc_flag, tdn_flag) %>%
  summarise(doc_mg_l = mean(doc_mg_l, na.rm = TRUE),
            tdn_mg_l = mean(tdn_mg_l, na.rm = TRUE)) %>%
  mutate(npoc_flag = case_when(doc_mg_l == 'NaN' ~ "replicates greater than 20% different"),
         tdn_flag = case_when(tdn_mg_l == 'NaN' ~ "replicates greater than 20% different"),
         date = stringr::str_extract(sample_name, "[0-9]{8}"))

# 7. Clean data ----------------------------------------------------------------

#Need to merge those:
npoc_dups_merged <- samples_dilution_corrected2_no_reps %>% 
  bind_rows(reps_clean)

## Flagging data
npoc_flags <- npoc_dups_merged %>% 
  ## add flags 
  # Below blank 
  mutate(npoc_flag = case_when(doc_mg_l == 'NaN'& sample_name %in% reps_clean$sample_name ~ "replicates greater than 20% different",
                               sample_name %in% reps_clean$sample_name ~ "average value of multiple aliquots",
                               doc_mg_l == 'NaN' ~ "value below blank",
                               grepl("POOL", sample_name) ~ "pooled sample",
                               grepl("pool", sample_name) ~ "pooled sample",
                               grepl("value above cal curve",npoc_flag) ~ "value above cal curve",
                               TRUE ~ npoc_flag), 
         tdn_flag = case_when(tdn_mg_l == 'NaN'& sample_name %in% reps_clean$sample_name ~ "replicates greater than 20% different",
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
         Timepoint = stringr::str_extract(sample_name,"[0-9]{8}|T[0-9]|HR[0-9]|PreW"),
         date = case_when(Grid =="SOURCE" ~ "20220622",
                          Grid == "BARGE" ~ "20220622",
                          TRUE ~ date),
         doc_mg_l = case_when(doc_mg_l == "NaN" ~ NA,
                              TRUE ~ doc_mg_l),
         tdn_mg_l = case_when(tdn_mg_l == "NaN" ~ NA,
                              TRUE ~ tdn_mg_l)) %>%
  filter(Grid != "SOURCE") %>%
  filter(Grid != "WELL") %>%
  filter(Grid != "BARGE") 


# 8. Write data ----------------------------------------------------------------

write_csv(npoc_wflags_metadata , "../TEMPEST-1-porewater/data/TMP_PW_NPOC_TDN_L1_May2022-May2023.csv")


