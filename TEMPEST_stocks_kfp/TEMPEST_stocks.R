library(tidyverse)

data = read_csv("UGASoilAnalyses_TEMPEST_082020.csv", skip = 8)

BD_A = 0.78 #g/cm3 A horizon -- 8 cm
BD_B = 1.49 # g/cm3 B horizon -- 15-25 cm

clean = 
  data %>% 
  dplyr::select(Sample, C, N) %>% 
  filter(!is.na(Sample)) %>% 
  separate(Sample, sep = " ", into = c("X1", "X2", "treatment", "plot", "depth")) %>% 
  dplyr::select(-X1, -X2) %>% 
#  mutate(depth = str_extract(Sample, "[0-9]{1,2}-[0-9]{1,2}")) %>% 
  separate(depth, sep = "-", into = c("depth_top", "depth_bottom")) %>% 
  mutate(depth_top = as.numeric(depth_top),
         depth_bottom = as.numeric(depth_bottom)) %>% 
  filter(depth_bottom <= 15) %>% 
  mutate(bulk_density = case_when(depth_top < 10 ~ BD_A,
                                  depth_top >= 10 ~ BD_B)) %>% 
  # calculate C per volume (g/cm3) and C stocks (g/cm2)
  # C g/cm3 = C (%) * BD (g/cm3)
  # C g/cm2 = C (%) * BD (g/cm3) * height (cm == 5 in this case)
  mutate(C_gcm3 = C * bulk_density,
         C_gcm2 = C * bulk_density * 5)

stocks_15cm = 
  clean %>% 
  group_by(treatment, plot) %>% 
  dplyr::summarise(C_stocks_15cm_gcm2 = sum(C_gcm2))

stocks_15cm %>% write.csv("carbon_stocks_top15cm.csv", row.names = F, na = "")