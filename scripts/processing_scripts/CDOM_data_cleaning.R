library(dplyr)
library(lubridate)
library(tidyverse)
library(car)
library(emmeans)
library(lme4)
library(lmerTest)  
library(ggplot2)    # For diagnostic plots
library(broom)      # For tidy model outputs

# double check your wd
getwd()

#clean out your environment 
rm(list = ls())

#### Set Study Dates ####
endstudydate = lubridate::as_date("2024-01-31")
startstudydate = lubridate::as_date("2022-05-01")

endstudydate_yr1 = lubridate::as_date("2023-05-31")
startstudydate_yr1 = lubridate::as_date("2022-05-01")

endstudydate_yr2 = lubridate::as_date("2024-05-31")
startstudydate_yr2 = lubridate::as_date("2023-05-01") 

endstudydate_yr3 = lubridate::as_date("2025-05-31")
startstudydate_yr3 = lubridate::as_date("2024-05-01") 

year1_start = lubridate::as_date("2022-01-01")
year1_stop = lubridate::as_date("2022-12-31")

year2_start = lubridate::as_date("2023-01-01")
year2_stop = lubridate::as_date("2023-12-31")

year3_start = lubridate::as_date("2024-01-01")
year3_stop = lubridate::as_date("2024-12-31")

EventStart2022 = lubridate::as_date("2022-06-22")

WaterDeliveryStart2022 = as.POSIXct("2022-06-22 05:30:00", tz = "EST")
WaterDeliveryStop2022 = as.POSIXct("2022-06-22 14:30:00", tz = "EST")

EventStart2023 = lubridate::as_date("2023-06-06")

WaterDeliveryStart1 = as.POSIXct("2023-06-06 05:30:00", tz = "EST")
WaterDeliveryStop1 = as.POSIXct("2023-06-06 14:30:00", tz = "EST")

WaterDeliveryStart2 = as.POSIXct("2023-06-07 05:30:00", tz = "EST")
WaterDeliveryStop2 = as.POSIXct("2023-06-07 14:30:00", tz = "EST")

EventStart2024 = lubridate::as_date("2024-06-11")

WaterDeliveryStart1_2024 = as.POSIXct("2024-06-11 06:10:00", tz = "EST")
WaterDeliveryStart2_2024 = as.POSIXct("2024-06-12 06:08:00", tz = "EST")
WaterDeliveryStart3_2024 = as.POSIXct("2024-06-13 06:21:00", tz = "EST")

plot_order <- c('Control', 'Freshwater','Saltwater')
time_order <- c('Pre', 'Mid','Post')

#### bring in DOC ####

#Porewater DOC data, from L1 google drive: 

doc_l1_2022 <- read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDSONLY_NPOCTDN_2022_L1.csv")

doc_l1_2023 <- read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDSONLY_NPOCTDN_2023_L1.csv")

doc_l1_2024 <- read_csv("~/GitHub/TEMPEST_Porewater/processing_scripts/DOC/TEMPORARY/01_TMP_PW_NPOCTDN_2024_L1.csv") %>%
  mutate(sample_name = stringr::str_remove(sample_name, "_DOC"))

doc_l1_2025 <- read_csv("~/GitHub/TEMPEST_Porewater/processing_scripts/DOC/TEMPORARY/01_TMP_PW_NPOCTDN_2025_L1.csv")

# pooled DOC data

pooled_DOC_l1_2022 <- read_csv("~/GitHub/TEMPEST_Porewater/processing_scripts/DOC/TEMPORARY/TMP_PW_POOLED_NPOCTDN_2022_L1.csv")
pooled_DOC_l1_2023 <- read_csv("~/GitHub/TEMPEST_Porewater/processing_scripts/DOC/TEMPORARY/TMP_PW_POOLED_NPOCTDN_2023_L1.csv")


#### bring in CDOM ####
#### filter to indicies that don't depend on the DOC concentration since this is not consistently matched over all the study years and is currently extremely difficult to pair.

cdom_2022 <- read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDS_POOL_Indicies_CDOM_2022_L1.csv") %>%
  filter(grid == "POOL") %>%
  mutate(A_T = A_RSU/T_RSU,
         C_A = C_RSU/A_RSU,
         C_M = C_RSU/M_RSU,
         C_T = C_RSU/T_RSU) %>%
  select(sample_name, plot, collection_datetime, S275_295, S350_400, Sr, FI, HIX, FRESH, BIX, A_T, C_A, C_M, C_T)

cdom_2023 <- read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDS_POOL_Indicies_CDOM_2023_L1.csv") %>%
  mutate(A_T = A_RSU/T_RSU,
         C_A = C_RSU/A_RSU,
         C_M = C_RSU/M_RSU,
         C_T = C_RSU/T_RSU) %>%
  select(sample_name, plot, collection_datetime, S275_295, S350_400, Sr, FI, HIX, FRESH, BIX, A_T, C_A, C_M, C_T)

cdom_2024_2025 <- readxl::read_excel("~/GitHub/TEMPEST_Porewater/processing_scripts/CDOM/TMP3_and_monthly_processed/TMP3_and_monthly_CDOM_indices_with_meta.xlsx") %>%
  select(SERC_inventory_id, DOC_mgL, SUVA254, SUVA280, SUVA350, SVA412, SVA440, S275_295:a254, pB, pT, pA, pM, pC, FI, HIX, fresh, BIX) %>%
  rename(B_RSU = pB,
         C_RSU = pC,
         A_RSU = pA,
         T_RSU = pT,
         M_RSU = pM,
         Sr = SR,
         FRESH = fresh) %>%
  mutate( A_T = A_RSU/T_RSU,
         C_A = C_RSU/A_RSU,
         C_M = C_RSU/M_RSU,
         C_T = C_RSU/T_RSU) %>%
  dplyr::mutate(Event = "TMP",
                Plot = stringr::str_extract(SERC_inventory_id, 'FW|SW|C|ESTUARY'), 
                Grid = stringr::str_extract(SERC_inventory_id, "B4|C3|C6|D5|E3|F4|F6|H3|H6|I5|SOURCE|BARGE|POOL|WELL"),
                date = stringr::str_extract(SERC_inventory_id, "[0-9]{8}"),
                time = stringr::str_extract(SERC_inventory_id, "(?<=[0-9]{8}_)\\d{4}"),
                time = str_replace(time, '\\d+', function(m) str_pad(m, 6, pad = '0', side = ("right")))) %>%
  mutate(date = lubridate::as_date(date, format = "%Y%m%d")) %>%
  rename(collection_datetime = date,
         sample_name = SERC_inventory_id)

cdom_2024 <- cdom_2024_2025 %>%
  select(sample_name, Plot, collection_datetime, S275_295, S350_400, Sr, FI, HIX, FRESH, BIX, A_T, C_A, C_M, C_T) %>%
  filter(collection_datetime > year3_start & collection_datetime < year3_stop) %>%
  mutate(sample_name = stringr::str_remove(sample_name, "_CDOM"),
         plot = case_when(Plot == "FW" ~ "Freshwater",
                          Plot == "C" ~ "Control",
                          Plot == "SW" ~ "Saltwater",
                          TRUE ~ Plot)) %>%
  select(-Plot)

cdom_2025 <- cdom_2024_2025 %>%
  select(sample_name, Plot, collection_datetime, S275_295, S350_400, Sr, FI, HIX, FRESH, BIX, A_T, C_A, C_M, C_T) %>%
  filter(collection_datetime > year3_stop & collection_datetime < endstudydate_yr3) %>%
  mutate(sample_name = stringr::str_remove(sample_name, "_CDOM"),
         plot = case_when(Plot == "FW" ~ "Freshwater",
                          Plot == "C" ~ "Control",
                          Plot == "SW" ~ "Saltwater",
                          TRUE ~ Plot)) %>%
  select(-Plot)

#### Year 1 CDOM signatures ####
pw_cdom_yr1 <- cdom_2022 %>%
  mutate(date = as_date(collection_datetime)) %>%
  filter(between(date, startstudydate_yr1, endstudydate)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  # fix the sampling dates in June before the event to the month before
  mutate(adj_month = case_when(adj_month == 1 & date < EventStart2022 ~ adj_month == 0,
                               TRUE ~ adj_month)) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295:C_T, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_cdom_yr1_2023 <- cdom_2023 %>%
  mutate(date = as_date(collection_datetime)) %>%
  filter(between(date, startstudydate_yr1, endstudydate_yr1)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  mutate(adj_month = case_when(adj_month == 0 ~ 12,
                               TRUE ~ adj_month )) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295:C_T, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_cdom_y1_all <- pw_cdom_yr1 %>%
  full_join(pw_cdom_yr1_2023)


saveRDS(pw_cdom_y1_all, "~/GitHub/TEMPEST-1-porewater/data/averaged_cdom_indicies_porewater_TMP1.rds")


#### Year 2 CDOM Signatures ####
pw_cdom_yr2 <- cdom_2023 %>%
  mutate(date = as_date(collection_datetime)) %>%
  filter(between(date, startstudydate_yr2, endstudydate_yr2)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  # fix the sampling dates in June before the event to the month before
  mutate(adj_month = case_when(adj_month == 1 & date < EventStart2023 ~ adj_month == 0,
                               TRUE ~ adj_month)) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295:C_T, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_cdom_yr2_2024 <- cdom_2024 %>%
  mutate(date = as_date(collection_datetime)) %>%
  filter(between(date, startstudydate_yr2, endstudydate_yr2)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  mutate(adj_month = case_when(adj_month == 0 ~ 12,
                               TRUE ~ adj_month )) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295:C_T, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_cdom_y2_all <- pw_cdom_yr2 %>%
  full_join(pw_cdom_yr2_2024)



saveRDS(pw_cdom_y2_all, "~/GitHub/TEMPEST-1-porewater/data/averaged_cdom_indicies_porewater_TMP2.rds")

#### Year 3 CDOM Signatures ####
pw_cdom_yr3 <- cdom_2024 %>%
  mutate(date = as_date(collection_datetime)) %>%
  filter(between(date, startstudydate_yr3, endstudydate_yr3)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  # fix the sampling dates in June before the event to the month before
  mutate(adj_month = case_when(adj_month == 1 & date < EventStart2023 ~ adj_month == 0,
                               TRUE ~ adj_month)) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295:C_T, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()


pw_cdom_yr3_2025 <- cdom_2025 %>%
  mutate(date = as_date(collection_datetime)) %>%
  filter(between(date, startstudydate_yr3, endstudydate_yr3)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  mutate(adj_month = case_when(adj_month == 0 ~ 12,
                               TRUE ~ adj_month )) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295:C_T, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_cdom_y3_all <- pw_cdom_yr3 %>%
  full_join(pw_cdom_yr3_2025)



saveRDS(pw_cdom_y3_all, "~/GitHub/TEMPEST-1-porewater/data/averaged_cdom_indicies_porewater_TMP3.rds")




#### DOC normalized optical properties ####

abs_doc_2022_pool <- read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDS_POOL_Indicies_CDOM_2022_L1.csv") %>%
  filter(grid == "POOL") %>%
  mutate(collection_date = as_date(collection_datetime)) %>%
  left_join(pooled_DOC_l1_2022, by = join_by(plot, grid, collection_date, evacuation_date) ) %>%
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295:FDOM_RSU) %>%
  filter(!is.na(S275_295)) %>% # remove DOC values when not matched by sample_name to a CDOM sample. Note there are several instances where the DOC values are recording "Lysimeter Empty" but yet we have CDOM data for these. Creating a github issue about this. 
  mutate(SUVA254 = Abs_254nm / doc_mg_l * 100, 
         SUVA280 = Abs_280nm / doc_mg_l * 100,
         SUVA350 = Abs_350nm / doc_mg_l * 100,
         SVA412 = Abs_412nm / doc_mg_l * 100,
         SVA440 = Abs_440nm / doc_mg_l * 100) %>% #fewsdom units in L/ mgC/m
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295, S350_400, Sr, Abs_250nm:Abs_440nm, SUVA254:SVA440)
#note there are quite a few dates where we have pooled CDOM data that we don't have pooled DOC data for. 
# Leave the DOC as NA for now and bring it in when you get to the plot means during analysis...

abs_doc_2023_grids <-  read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDS_POOL_Indicies_CDOM_2023_L1.csv")  %>%
  select(-collection_datetime) %>%
  filter(grid != "POOL") %>%
  full_join(doc_l1_2023, by = join_by(plot, grid, sample_name) ) %>%
  select(sample_name, plot, grid, evacuation_datetime, collection_datetime, doc_mg_l, S275_295:FDOM_RSU) %>%
  filter(!is.na(S275_295)) %>% # remove DOC values when not matched by sample_name to a CDOM sample. Note there are several instances where the DOC values are recording "Lysimeter Empty" but yet we have CDOM data for these. Creating a github issue about this. 
  mutate(SUVA254 = Abs_254nm / doc_mg_l * 100, 
         SUVA280 = Abs_280nm / doc_mg_l * 100,
         SUVA350 = Abs_350nm / doc_mg_l * 100,
         SVA412 = Abs_412nm / doc_mg_l * 100,
         SVA440 = Abs_440nm / doc_mg_l * 100) %>% #fewsdom units in L/ mgC/m
  mutate(collection_date = as_date(collection_datetime)) %>%
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295, S350_400, Sr, Abs_250nm:Abs_440nm, SUVA254:SVA440)

abs_doc_2023_pool <-  read_csv("~/GitHub/TEMPEST-1-porewater/processed data/for_google_drive/TMP_PW_GRIDS_POOL_Indicies_CDOM_2023_L1.csv")  %>%
  filter(grid == "POOL") %>%
  mutate(collection_date = as_date(collection_datetime)) %>%
  left_join(pooled_DOC_l1_2023, by = join_by(plot, grid, collection_date, evacuation_date) ) %>%
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295:FDOM_RSU) %>%
  filter(!is.na(S275_295)) %>% # remove DOC values when not matched by sample_name to a CDOM sample. Note there are several instances where the DOC values are recording "Lysimeter Empty" but yet we have CDOM data for these. Creating a github issue about this. 
  mutate(SUVA254 = Abs_254nm / doc_mg_l * 100, 
         SUVA280 = Abs_280nm / doc_mg_l * 100,
         SUVA350 = Abs_350nm / doc_mg_l * 100,
         SVA412 = Abs_412nm / doc_mg_l * 100,
         SVA440 = Abs_440nm / doc_mg_l * 100) %>% #fewsdom units in L/ mgC/m
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295, S350_400, Sr, Abs_250nm:Abs_440nm, SUVA254:SVA440)

abs_norm_2023 <- abs_doc_2023_grids %>%
  full_join(abs_doc_2023_pool)  %>%
  filter(!is.na(doc_mg_l))

abs_norm_2024 <-  cdom_2024_2025 %>%
  rename(collection_date = collection_datetime,
         grid = Grid,
         doc_mg_l = DOC_mgL) %>%
  filter(collection_date > year3_start & collection_date < year3_stop) %>%
  mutate(sample_name = stringr::str_remove(sample_name, "_CDOM"),
         plot = case_when(Plot == "FW" ~ "Freshwater",
                          Plot == "C" ~ "Control",
                          Plot == "SW" ~ "Saltwater",
                          TRUE ~ Plot)) %>%
  select(-Plot) %>%
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295, S350_400, Sr, SUVA254:SVA440)

abs_norm_2025 <-  cdom_2024_2025 %>%
  filter(collection_datetime > year3_stop & collection_datetime < endstudydate_yr3) %>%
  rename(collection_date = collection_datetime,
         grid = Grid,
         doc_mg_l = DOC_mgL) %>%
  mutate(sample_name = stringr::str_remove(sample_name, "_CDOM"),
         plot = case_when(Plot == "FW" ~ "Freshwater",
                          Plot == "C" ~ "Control",
                          Plot == "SW" ~ "Saltwater",
                          TRUE ~ Plot)) %>%
  select(-Plot) %>%
  select(sample_name, plot, grid, collection_date, doc_mg_l, S275_295, S350_400, Sr, SUVA254:SVA440)

#### Year 1 ABS signatures ####
pw_abs_yr1 <- abs_doc_2022_pool%>%
  mutate(date = as_date(collection_date)) %>%
  filter(between(date, startstudydate_yr1, endstudydate)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  # fix the sampling dates in June before the event to the month before
  mutate(adj_month = case_when(adj_month == 1 & date < EventStart2022 ~ adj_month == 0,
                               TRUE ~ adj_month)) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295, S350_400, Sr, SUVA254:SVA440, Abs_250nm:Abs_440nm, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_abs_yr1_2023 <- abs_norm_2023 %>%
  mutate(date = as_date(collection_date)) %>%
  filter(between(date, startstudydate_yr1, endstudydate_yr1)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  mutate(adj_month = case_when(adj_month == 0 ~ 12,
                               TRUE ~ adj_month )) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295, S350_400, Sr, SUVA254:SVA440, Abs_250nm:Abs_440nm, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_abs_y1_all <- pw_abs_yr1 %>%
  full_join(pw_abs_yr1_2023)


saveRDS(pw_abs_y1_all, "~/GitHub/TEMPEST-1-porewater/data/averaged_doc_norm_abs_indicies_porewater_TMP1.rds")


#### Year 2 ABS Signatures ####
pw_abs_yr2 <- abs_norm_2023 %>%
  mutate(date = as_date(collection_date)) %>%
  filter(between(date, startstudydate_yr2, endstudydate_yr2)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  # fix the sampling dates in June before the event to the month before
  mutate(adj_month = case_when(adj_month == 1 & date < EventStart2023 ~ adj_month == 0,
                               TRUE ~ adj_month)) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295, S350_400, Sr, SUVA254:SVA440, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_abs_yr2_2024 <- abs_norm_2024 %>%
  mutate(date = as_date(collection_date)) %>%
  filter(between(date, startstudydate_yr2, endstudydate_yr2)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  mutate(adj_month = case_when(adj_month == 0 ~ 12,
                               TRUE ~ adj_month )) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295, S350_400, Sr, SUVA254:SVA440, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_abs_y2_all <- pw_abs_yr2 %>%
  full_join(pw_abs_yr2_2024)



saveRDS(pw_abs_y2_all, "~/GitHub/TEMPEST-1-porewater/data/averaged_doc_norm_abs_indicies_porewater_TMP2.rds")

#### Year 3 ABS Signatures ####
pw_abs_yr3 <- abs_norm_2024 %>%
  mutate(date = as_date(collection_date)) %>%
  filter(between(date, startstudydate_yr3, endstudydate_yr3)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  # fix the sampling dates in June before the event to the month before
  mutate(adj_month = case_when(adj_month == 1 & date < EventStart2023 ~ adj_month == 0,
                               TRUE ~ adj_month)) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295, S350_400, Sr, SUVA254:SVA440, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()


pw_abs_yr3_2025 <- abs_norm_2025 %>%
  mutate(date = as_date(collection_date)) %>%
  filter(between(date, startstudydate_yr3, endstudydate_yr3)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date),
         # Adjust months: May = 0, June = 1, ..., April = 11
         adj_month = (month(date) - 5) %% 12 ) %>%
  mutate(adj_month = case_when(adj_month == 0 ~ 12,
                               TRUE ~ adj_month )) %>%
  group_by(adj_month, year, plot) %>%
  dplyr::select(plot, S275_295, S350_400, Sr, SUVA254:SVA440, adj_month, year) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))), .groups = "keep") %>%
  ungroup()

pw_abs_y3_all <- pw_abs_yr3 %>%
  full_join(pw_abs_yr3_2025)



saveRDS(pw_abs_y3_all, "~/GitHub/TEMPEST-1-porewater/data/averaged_doc_norm_abs_indicies_porewater_TMP3.rds")




