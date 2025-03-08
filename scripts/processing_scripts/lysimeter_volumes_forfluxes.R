# Total Lysimeter Volumes 

# This script creates a single dataframe with all lysimeter volumes across grids and pooled samples. 

require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               googlesheets4, # read_sheet 
               googledrive, # drive_ functions
               here, #helps with file pathing 
               lubridate) #those pesky dates

#double check your wd
getwd()

#Authenticate Google Drive Access:
drive_auth()

Sys.setenv(TZ = "America/New_York")

#set the study dates: 
year1_start = lubridate::as_date("2022-01-01")
year1_stop = lubridate::as_date("2022-12-31")

year2_start = lubridate::as_date("2023-01-01")
year2_stop = lubridate::as_date("2023-12-31")

year3_start = lubridate::as_date("2024-01-01")
year3_stop = lubridate::as_date("2024-12-31")

## Get Volumes from Pooled samples from metadata sheet per grid:

inventory_directory <- "https://docs.google.com/spreadsheets/d/1sFWq-WKhemPzbOFInqhCu_Lx0lsO6a_Z/edit#gid=496164093"

directory= file.path(here() %>% dirname(), 'TEMPEST-1-Porewater/data_do_not_commit/water fluxes')

file_path = file.path(directory,"porewaterinventory.xlsx")

drive_download(inventory_directory, path= file_path, overwrite = TRUE)

pooled_vols <- readxl::read_excel(file_path, skip=4, sheet="Porewater - Pooled") %>%
  select(Sample_ID, Total_Volume_mL, Grid_C3:Grid_I5, Evacuation_date_YYYMMDD, Collection_Date_YYYYMMDD) %>%
  mutate(across(starts_with("Grid"), ~ ifelse(is.na(.),0,.))) %>%
  mutate(Plot = stringr::str_extract(Sample_ID, 'FW|SW|C')) %>%
  pivot_longer(cols = c("Grid_C3", "Grid_C6", "Grid_F4", "Grid_H3", "Grid_H6", "Grid_B4", "Grid_D5", "Grid_E3", "Grid_F6", "Grid_I5"),
               names_to = "grid",
               values_to = "Volume_mL") %>%
  mutate(plot = case_when(Plot == "C" ~ "Control",
                          Plot == "SW" ~ "Estuarine-water",
                          Plot == "FW" ~ "Freshwater",
                          Plot == "Saltwater" ~ "Estuarine-water",
                          TRUE ~ Plot),
         grid = str_remove(grid, "Grid_")) %>%
  rename(evacuation_date = Evacuation_date_YYYMMDD,
         collection_date = Collection_Date_YYYYMMDD) %>%
  mutate(evacuation_date = lubridate::as_date(as.character(evacuation_date), format = "%Y%m%d"),
         collection_date = lubridate::as_date(as.character(collection_date), format = "%Y%m%d"),
         date = stringr::str_extract(Sample_ID, "[0-9]{8}")) %>%
  mutate(date = lubridate::as_date(date, format = "%Y%m%d")) %>%
  group_by(date) %>%
  #manual entry of missing evacuation information from the notes: 
  mutate(evacuation_date = case_when(
    is.na(evacuation_date) & str_detect(collection_date, "2022-05-18") ~ as.Date("2022-05-12"),
    is.na(evacuation_date) & str_detect(collection_date, "2022-05-20")  ~ as.Date("2022-05-12"), 
    is.na(evacuation_date) & str_detect(collection_date, "2022-06-13")  ~ as.Date("2022-06-09"),
    is.na(evacuation_date) & str_detect(collection_date, "2022-06-15")  ~ as.Date("2022-06-09"),
    is.na(evacuation_date) & str_detect(collection_date, "2022-06-20") ~ as.Date("2022-06-18"),
    is.na(evacuation_date) & str_detect(collection_date, "2022-06-22") ~ as.Date("2022-06-22"),
    is.na(evacuation_date) & str_detect(collection_date, "2022-06-23") ~ as.Date("2022-06-22"),
    is.na(evacuation_date) & str_detect(collection_date, "2022-06-24") ~ as.Date("2022-06-23"),
    is.na(evacuation_date) & str_detect(collection_date, "2023-06-05") ~ as.Date("2023-06-03"),
    is.na(evacuation_date) & str_detect(collection_date, "2023-10-05") ~ as.Date("2023-10-02"),
    TRUE ~ evacuation_date)) %>%
  fill(evacuation_date, collection_date) %>%
  #Empty dates for the event dates, those are when collection and evacuation are usually the same date: 
  mutate(collection_date = case_when(is.na(collection_date) ~ date,
                                     TRUE ~ collection_date),
         evacuation_date = case_when(is.na(evacuation_date) ~ date,
                                     TRUE ~ evacuation_date)) %>%
  ungroup() %>%
  select(plot, grid, Volume_mL, evacuation_date, collection_date) %>%
  filter(Volume_mL > 0) %>%
  rename(Volume_from_Pooled_mL=Volume_mL) %>%
  filter(collection_date < year2_stop & collection_date > year1_start)

# Grid level volumes for all aliquots was generated in 1_porewater_doc.Rmd, source the output from this. 

grid_vols = read_rds("~/GitHub/TEMPEST-1-porewater/processed data/TMP_PW_Aliquots_Volumes.rds") %>%
  rename(plot = Plot,
         grid = Grid) %>%
  filter(collection_date < year2_stop & collection_date > year1_start)
  

# 2022 event specific volumes: 

vols_2022event <- read_csv("~/GitHub/tempest-system-level-analysis/data/raw/DOC meta/TMP_Event_June2022_gridlevel_volumes.csv") %>%
  select(Plot, Timepoint, Date, Grid, Total_Volume) %>%
  rename(plot = Plot,
         grid = Grid,
         Total_Volume_mL = Total_Volume,
         collection_date = Date) %>%
  mutate(evacuation_date = case_when(
    str_detect(collection_date, "2022-06-20") ~ as.Date("2022-06-18"),
    str_detect(collection_date, "2022-06-22") ~ as.Date("2022-06-22"),
    str_detect(collection_date, "2022-06-23") ~ as.Date("2022-06-22"),
    str_detect(collection_date, "2022-06-24") ~ as.Date("2022-06-23"),
    TRUE ~ NA)) %>%
  drop_na() %>%
  select(plot, grid, evacuation_date, collection_date, Total_Volume_mL) %>%
  mutate(plot = case_when(plot == "C" ~ "Control",
                          plot == "SW" ~ "Estuarine-water",
                          plot == "FW" ~ "Freshwater",
                          TRUE ~ plot))
  

#combine:

volumes_grids_pooled <- grid_vols %>%
  full_join(pooled_vols, by= c("plot", "grid", "evacuation_date", "collection_date")) %>%
  distinct() %>%
  mutate(across(contains("Volume"), ~ ifelse(is.na(.),0,.))) %>%
  mutate(Total_Volume_mL = sum(Total_Aliquot_Volume_mL, Volume_from_Pooled_mL)) %>%
  select(plot, grid, evacuation_date, collection_date, Total_Volume_mL) %>%
  full_join(vols_2022event, by= c("plot", "grid", "evacuation_date", "collection_date"), suffix = c("", ".fill")) %>%
  mutate(Total_Volume_mL = coalesce(Total_Volume_mL, Total_Volume_mL.fill)) %>%
  select(-Total_Volume_mL.fill) %>%
  arrange(collection_date)

saveRDS(volumes_grids_pooled, "~/GitHub/TEMPEST-1-porewater/processed data/TMP_PW_Total_Volumes.rds")

rm(grid_vols, pooled_vols, vols_2022event)

