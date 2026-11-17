# Read in various experiments
library(tidyverse)
library(readxl)

all_experiments <- read_csv("BHM/Data/TEMPEST_lab_experiments_all_L2.csv")

all_experiments_cl_modeled <- all_experiments %>% 
  ungroup() %>% 
  mutate(
    Na_charge = Na_ppm / 22.99 * 1,
    K_charge  = K_ppm  / 39.10 * 1,
    Ca_charge = Ca_ppm / 40.08 * 2,
    Mg_charge = Mg_ppm / 24.31 * 2,
    Al_charge = Al_ppm / 26.98 * 3,
    Fe_charge = Fe_ppm / 55.85 * 3,   # Fe³⁺
    S_charge  = S_ppm  / 96.06 * 2    # SO₄²⁻ (ppm as sulfate)
  ) %>%
  rowwise() %>%
  mutate(Cl_charge = sum(c_across(Na_charge:Fe_charge) - S_charge),
         Cl_ppm = Cl_charge * 35.45 * 1
         )

ggplot(all_experiments, aes(x = Timepoint, y = doc_mgperL,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type) +
  scale_y_log10()

ggplot(all_experiments, aes(x = Timepoint, y = doc_mgperL,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type)

ggplot(all_experiments, aes(x = Timepoint, y = Ca_ppm,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type) +
  scale_y_log10()

ggplot(all_experiments %>% filter(Timepoint == 1), aes(x = Timepoint, y = Ca_ppm,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type) +
  scale_y_log10()

ggplot(all_experiments, aes(x = Timepoint, y = Ca_ppm,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type)

ggplot(all_experiments, aes(x = Timepoint, y = conductivity_uSpercm,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type) +
  scale_y_log10()

all_experiments_sum_sum <- read_csv("BHM/Data/TEMPEST_lab_experiments_all_L2.csv") %>% 
  filter(Timepoint  >= 1) %>% 
  # mutate(Column = ifelse(is.na(Column), 1, Column)) %>% 
  group_by(Treatment, Exp_Type, Column) %>% 
  #summarise(conductivity_uSpercm, Na_ppm) %>% 
  mutate(cumsum_doc = cumsum(doc_mgperL))

ggplot(all_experiments_sum_sum, aes(x = Timepoint, y = cumsum_doc,
                                    color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type)

treatment_waters <- all_experiments_cl_modeled %>% 
  filter(Timepoint == 0) %>% 
  select(-c(Timepoint, doc_mgperL, Column)) %>% 
  mutate(
    Na = Na_ppm / 22.99 / 1000 * 1^2,
    K  = K_ppm  / 39.10 / 1000 * 1^2,
    Ca = Ca_ppm / 40.08 / 1000 * 2^2,
    Mg = Mg_ppm / 24.31 / 1000 * 2^2,
    Al = Al_ppm / 26.98 / 1000 * 3^2,
    Fe = Fe_ppm / 55.85 / 1000 * 3^2,   # Fe³⁺
    S  = S_ppm  / 96.06 / 1000 * 2^2,    # SO₄²⁻ (ppm as sulfate)
    Cl = Cl_ppm / 35.45 / 1000 * 1^2
  ) %>%
  rowwise() %>%
  mutate(I = 0.5 * sum(c_across(Na:S)))

all_experiments_sum2 <- read_csv("BHM/Data/TEMPEST_lab_experiments_all_L2.csv") %>% 
  filter(Timepoint  >= 1 & Timepoint  <= 5) %>% 
  group_by(Treatment, Column, Exp_Type) %>% 
  summarise(doc_mgperL = sum(doc_mgperL),
            n = n()) %>% 
  left_join(treatment_waters)

ggplot(all_experiments_sum2, aes(x = conductivity_uSpercm, y = doc_mgperL)) +
  geom_point(aes(color =  Treatment)) +
  facet_wrap(.~Exp_Type)

ggplot(all_experiments_sum2, aes(x = I, y = doc_mgperL)) +
  geom_point(aes(color = Exp_Type)) +
  #facet_wrap(.~Exp_Type) +
  scale_x_log10() +
  scale_y_log10()

# Start managing data

# Jaggify 
all_experiments_jaggified <- all_experiments_cl_modeled %>% 
 # filter(Timepoint > 0) %>% 
  ungroup() %>% 
  mutate(
    Na = Na_ppm / 22.99 / 1000 * 1^2,
    K  = K_ppm  / 39.10 / 1000 * 1^2,
    Ca = Ca_ppm / 40.08 / 1000 * 2^2,
    Mg = Mg_ppm / 24.31 / 1000 * 2^2,
    Al = Al_ppm / 26.98 / 1000 * 3^2,
    Fe = Fe_ppm / 55.85 / 1000 * 3^2,   # Fe³⁺
    S  = S_ppm  / 96.06 / 1000 * 2^2,    # SO₄²⁻ (ppm as sulfate)
    Cl = Cl_ppm / 35.45 / 1000 * 1^2
  ) %>%
  rowwise() %>%
  mutate(I = 0.5 * sum(c_across(Na:S))) %>% 
  group_by(Exp_Type, Treatment, Column) %>% 
  mutate(run_id = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(Ca_mmolL = Ca_ppm / 40.08)

ggplot(all_experiments_jaggified, aes(x = Timepoint, y = I,
                            color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type) +
  scale_y_log10()

ggplot(all_experiments_jaggified, aes(x = Timepoint, y = I,
                                      color = as.character(Treatment))) +
  geom_point(aes(size = Ca_ppm)) +
  geom_line(aes(group = paste0(Treatment, Column, sep="-"))) +
  facet_wrap(.~Exp_Type)

ggplot(all_experiments_jaggified %>% filter(Timepoint == 0), aes(x = conductivity_uSpercm, y = I)) +
  geom_point(aes(color = Exp_Type)) +
  scale_x_log10() +
  scale_y_log10(labels = scales::comma_format())

ggplot(all_experiments_jaggified, aes(x = conductivity_uSpercm, y = Ca_mmolL)) +
  geom_point(aes(color = Exp_Type)) +
  scale_x_log10() +
  scale_y_log10(labels = scales::comma_format())

ggplot(all_experiments_jaggified, aes(x = conductivity_uSpercm, y = Fe_mmolL)) +
  geom_point(aes(color = Exp_Type)) +
  scale_x_log10() +
  scale_y_log10(labels = scales::comma_format())

# Initial concentration
initial_concentration <- all_experiments_jaggified %>% 
  filter(Timepoint == 0)

all_experiments_jaggified2 <- all_experiments_jaggified %>% 
  filter(Timepoint > 0) %>% 
  group_by(Column) %>% 
  mutate(column_id = cur_group_id())

library(jags) 

init_conditions <- read_csv("BHM/Data/TEMPEST_soildata_2020.csv")  %>% 
  mutate(
  #  Na = Na_ppm / 22.99 / 1000 * 1^2,
  #  K  = K_ppm  / 39.10 / 1000 * 1^2,
    Ca = Ca_ppm / 40.08 / 1000 * 2^2,
  #  Mg = Mg_ppm / 24.31 / 1000 * 2^2,
    # Al = Al_ppm / 26.98 / 1000 * 3^2,
    Fe = Fe_ppm / 55.85 / 1000 * 3^2,   # Fe³⁺
   # S  = S_ppm  / 96.06 / 1000 * 2^2,    # SO₄²⁻ (ppm as sulfate)
   #  Cl = Cl_ppm / 35.45 / 1000 * 1^2
  ) %>%
  rowwise() %>%
 # mutate(I = 0.5 * sum(c_across(Na:S))) %>% 
  mutate(Ca_mmolL = Ca_ppm / 40.08 / 1000 * 1000,
         Fe_mmolL = Fe_ppm / 55.85 / 1000 * 1000)
  
# 5 cm diameter, 0–15 cm
core_vol <- 15*pi*2.5^2
# C pct 4.341266667 %>% 2.58785199
# 239.022 mean grams 
core_bd <- 239.022 / core_vol
# 20% physically available

# 1,000 mg / 1 g
# 1,000 cc / L

carbon_mg_L <- carbon_g_cc * 1000000 * 0.2

carbon_g_cc <- core_bd * 0.0434

porosity <- core_bd / 2.65

all_experiments_min_and_max <- all_experiments_jaggified %>% 
  filter(Timepoint != 0) %>% 
  group_by(Exp_Type) %>% 
  mutate(min_time = min( Timepoint),
         max_time = max( Timepoint)) %>% 
  ungroup() %>%
  filter(Timepoint == min_time |
           Timepoint == max_time) %>% 
  mutate(min_or_max = ifelse(Timepoint == min_time, "min", "max")) %>% 
  select(Exp_Type, Treatment, Column,
         conductivity_uSpercm, min_or_max) %>% 
  pivot_wider(names_from = min_or_max, values_from = conductivity_uSpercm,
              names_prefix = "conductivity_") %>% 
  mutate(Exp_Type = recode(Exp_Type,
                           "ASW" = "Batch - Artificial saltwater",
                           "ColEx" = "Column experiment",
                           "NaCl" = "Batch - NaCl"),
         Treatment = recode(Treatment, "ASW" = "Artificial saltwater"),
         Treatment = factor(Treatment, levels = c("0", "0.1", "1", "5", "25","100",
                                                  "Control", "Freshwater", "Artificial saltwater"
         ))
  )



# Lines for different treatments
# Color scheme that matches
# Shapes for ASW and NaCl

batch_treatments <- RColorBrewer::brewer.pal(6, "YlOrRd")

base_text_size <- 11
axis_text_size <- 10
axis_title_size <- 11
legend_text_size <- 10
panel_label_size <- 11

ggplot(all_experiments_min_and_max, aes(x = conductivity_min, y = conductivity_max)) +
  geom_point(aes(color = Treatment, shape = Exp_Type), size = 2) +
  geom_smooth(method = "lm", aes(group = Exp_Type, lty = Exp_Type), color="black") +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Initial Treatment Conductivity (µS/cm)") +
  ylab("Final Conductivity (µS/cm)") +
  scale_color_manual(values = c(batch_treatments,
                                "springgreen2", 
                                "cyan2",
                                "violetred2"
  )) +
  scale_fill_manual(values = c(batch_treatments,
                               "springgreen2",
                               "cyan2",
                               "violetred2"
  )) +
  theme_classic() +
  theme(legend.position = "right", panel.border = element_rect(color = "black", fill = NA, size = 2),
        
        text= element_text(size = 14)) +
  
  theme(
    
    legend.position = "right",
    
    legend.title = element_blank(),
    
    legend.text = element_text(size = legend_text_size),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    text = element_text(size = base_text_size),
    
    axis.text = element_text(size = axis_text_size),
    
    axis.title = element_text(size = axis_title_size),
    
    axis.title.x = element_text(size = axis_title_size),
    
    axis.title.y.right = element_text(color = "transparent", size = axis_title_size),
    
    axis.text.x = element_text(size = axis_text_size),
    
    axis.text.y.right = element_blank(),
    
    axis.ticks.y.right = element_blank(),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    plot.margin = margin(5.5, 5.5, 2, 5.5)
    
  )

ggsave("BHM/output/Init Versus Final Cond.jpg", width = 6, height = 4)
  
  # ggtitle("Saltier soil treatments stay saltier\nin response to flooding")

all_experiments_init <- all_experiments_jaggified %>% 
  filter(Timepoint == 1)

ggplot(all_experiments_init, aes(x = conductivity_uSpercm, y = Ca_ppm)) +
  geom_point(aes(color = as.character(Treatment), shape = Exp_Type)) +
  scale_x_log10() +
  scale_y_log10() +
  # xlab("Conductivity first treatment") +
  # ylab("Conductivity asymptote") +
  theme(legend.title = element_blank())
  # ggtitle("Saltier soil treatments stay saltier\nin response to flooding")

ggplot(all_experiments_init, aes(x = conductivity_uSpercm, y = doc_mgperL)) +
  geom_point(aes(color = as.character(Treatment))) +
  scale_x_log10() +
  scale_y_log10() +
  # xlab("Conductivity first treatment") +
  # ylab("Conductivity asymptote") +
  theme(legend.title = element_blank())
# ggtitle("Saltier soil treatments stay saltier\nin response to flooding")


NaCl_init <- all_experiments_init %>% 
  filter(Exp_Type == "NaCl")
  

lm1 <- lm(log(Ca_ppm)~log(conductivity_uSpercm), data = NaCl_init)
summary(lm1)

