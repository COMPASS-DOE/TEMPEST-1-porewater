# Summary 

library(arrow)
library(tidyverse)

tidy_jags <- read_parquet("BHM/output/tidy_jags.parquet")

tidy_jags_sumary <- tidy_jags %>% 
  group_by(Parameter) %>% 
  summarise(mean = mean(value),
            sd = sd(value))

write_csv(tidy_jags_sumary, "BHM/output/posterior_summary.csv")
View(tidy_jags_sumary)
