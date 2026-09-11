# 
library(rjags)
library(tidyverse)
library(ggmcmc)

all_experiments <- read_csv("BHM/Data/TEMPEST_lab_experiments_all_L2.csv")

# Do some internal management so that each column has its own control file
col_ex <- all_experiments %>% filter(Exp_Type == "ColEx")

treatments <- col_ex %>% 
  filter(Timepoint == 0) %>% 
  select(-Column)

columns_info <- col_ex %>% 
  filter(Timepoint != 0) %>% 
  select(Exp_Type, Treatment, Column) %>% 
  distinct_all() %>% 
  left_join(treatments)
  
col_ex_expanded <- col_ex %>% 
  filter(Timepoint != 0) %>%
  bind_rows(columns_info) %>% 
  arrange(Exp_Type, Treatment, Column, Timepoint)

# Start with just calcium soluability in the ASW batch experiments
all_experiments_simplest <- all_experiments %>% 
  filter(Exp_Type %in% c("ASW", "NaCl")) %>% 
  bind_rows(col_ex_expanded) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Control", "Freshwater", "ASW",
                                                     "0", "0.1", "1", "5", "25", "100"))) %>% 
 # group_by(Treatment) %>% 
  group_by(Exp_Type, Treatment, Column) %>% 
  mutate(treatment_id = cur_group_id()) %>% 
  ungroup()

treatment_solutions <- all_experiments_simplest %>% 
  group_by(treatment_id) %>% 
  mutate(n = n()-1) %>% 
  filter(Timepoint == 0) %>% 
  select(Exp_Type, Column, Treatment, treatment_id, 
         Ca_ppm, conductivity_uSpercm, n) %>% 
  mutate(is_nacl = ifelse(Exp_Type == "NaCl", 1, 0),
         exp_type_code = as.integer(ifelse(Exp_Type == "ColEx", 2, 1)))

response_Ca <- all_experiments_simplest %>% 
  filter(Timepoint != 0) %>% 
  mutate(Ca_ppm = ifelse(Ca_ppm == 0, 0.01, Ca_ppm)) %>% 
  select(treatment_id, 
         Timepoint,
         Ca_ppm) %>% 
  pivot_wider(values_from = Ca_ppm, names_from = treatment_id) %>% 
  select(-Timepoint)

response_cond <- all_experiments_simplest %>% 
  filter(Timepoint != 0) %>% 
  select(treatment_id, 
         Timepoint,
         conductivity_uSpercm) %>% 
  pivot_wider(values_from = conductivity_uSpercm, names_from = treatment_id) %>% 
  select(-Timepoint)

response_doc <- all_experiments_simplest %>% 
  filter(Timepoint != 0) %>% 
  select(treatment_id, 
         Timepoint,
         doc_mgperL) %>% 
  pivot_wider(values_from = doc_mgperL, names_from = treatment_id) %>% 
  select(-Timepoint)

jags_data <- list(Ca_obs = as.matrix(response_Ca),
                  cond_obs = as.matrix(response_cond),
                  DOC_obs = as.matrix(response_doc),
                  Ca_flood = treatment_solutions$Ca_ppm,
                  cond_flood = treatment_solutions$conductivity_uSpercm,
                  # treatment_id = treatment_solutions$treatment_id,
                  N_obs = treatment_solutions$n,
                  is_nacl = treatment_solutions$is_nacl,
                  exp_type_code = treatment_solutions$exp_type_code,
                  n_experiments = max(treatment_solutions$treatment_id)
                  )

Ca_flush_model <- "model {
  
  # PRIORS
  # flush rates
  # Different flush rates for different experiments
  for (j in 1:2) {
    lambda_Ca[j]  ~ dbeta(2, 2)
    lambda_cond[j] ~ dbeta(2, 2)
    lambda_DOC[j] ~ dbeta(2, 2)
  }

  # Initial Ca in porewater
  Ca_a ~ dnorm(0, 0.001) T(0,)
  Ca_b ~ dnorm(0, 0.001)
  
  cond_init ~ dnorm(0, 0.001) T(0,) 
  
  # Equation for conductivity aymptote
  cond_a ~ dnorm(0, 0.001) T(0,)
  cond_b ~ dnorm(0, 0.001)
 
  # Ca asymptote for ASW treatments
  Ca_inf_ASW ~ dnorm(0, 0.001) T(0,)  
  Ca_inf_NaCl <- 0.01
  
  # Observation noise
  sigma_Ca ~ dexp(5)
  sigma_cond ~ dexp(5)
  sigma_DOC ~ dexp(5)
  
  # Carbon model
  DOC_max ~ dnorm(0, 0.001) T(0,)
  k_m ~ dnorm(0, 0.001) T(0,)
  DOC_init ~ dnorm(0, 0.001) T(0,)
  
  DOC_c ~ dnorm(0, 1)
  DOC_b ~ dnorm(0, 1) T(0,)
  DOC_a ~ dnorm(0, 1) T(0,)
  
  DOC_inf ~ dnorm(0, 1) T(0,)
  
  for (c in 1:n_experiments) {
    
    # Calcium flushing
    Ca_init[c] <- exp(Ca_a * log(cond_flood[c]) +  Ca_b)
    
    Ca_inf[c] <- is_nacl[c]*Ca_inf_NaCl + (1-is_nacl[c]) *Ca_inf_ASW
    
    Ca_mobile[1, c] <- (Ca_flood[c] - Ca_inf[c]) + Ca_init[c]
    
    Ca_pw[1, c] <- Ca_mobile[1, c] + Ca_inf[c]
    
    # Log-normal likelihood
    Ca_obs[1, c] ~ dlnorm(log(Ca_pw[1, c]), 1 / sigma_Ca^2)
    
  
    # Conductivity flusing 
    cond_inf[c] <- exp(cond_a * log(cond_flood[c]) + cond_b)
    
    cond_mobile[1, c] <- (cond_flood[c] - cond_inf[c]) + cond_init
    
    cond_pw[1, c] <- cond_mobile[1, c] + cond_inf[c]
    
    # Log-normal likelihood
    cond_obs[1, c] ~ dlnorm(log(cond_pw[1, c]), 1 / sigma_cond^2)
  
    # Carbon module 
    # Carbon pool is initial carbon + carbon released from flooding treatment
    DOC_pool[c] <- DOC_init + (DOC_max * cond_flood[c] / (k_m + cond_flood[c]))
    
    DOC_R_logit[1, c] <-  DOC_c + DOC_b * Ca_pw[1, c] + DOC_a * cond_pw[1, c]  
    DOC_R[1, c] <- DOC_pool[c]/(1+exp(-DOC_R_logit[1, c]))
    
    DOC_mobile[1, c] <-  DOC_pool[c] - DOC_R[1, c]
    DOC_pw[1, c] <- DOC_mobile[1, c] + DOC_inf
    
    DOC_obs[1, c] ~ dlnorm(log(DOC_pw[1, c]), 1 / sigma_DOC^2)
    
    # Establish 
  
    for (i in 2:N_obs[c]) {

      # Ca pool updates
      Ca_mobile[i, c] <- Ca_mobile[i-1, c] * lambda_Ca[exp_type_code[c]]

      # Porewater Ca — mobile + exchange
      Ca_pw[i, c] <- Ca_mobile[i, c] + Ca_inf[c]
  
      # Log-normal likelihood
      Ca_obs[i, c] ~ dlnorm(log(Ca_pw[i, c]), 1 / sigma_Ca^2)
      
      # Conductivity pool pudates 
      cond_mobile[i, c] <- cond_mobile[i-1, c] * lambda_cond[exp_type_code[c]]
    
      cond_pw[i, c] <- cond_mobile[i, c] + cond_inf[c]
    
      # Log-normal likelihood
      cond_obs[i, c] ~ dlnorm(log(cond_pw[i, c]), 1 / sigma_cond^2)
      
      # DOC pool
      DOC_R_logit[i, c] <-  DOC_c + DOC_b * Ca_pw[i, c] + DOC_a * cond_pw[i, c]  
      DOC_R[i, c] <- DOC_pool[c]/(1+exp(-DOC_R_logit[i, c]))
      
      # Calculate the pool that moves from previous resistant pool to new mobile pool
      DOC_R_moving[i,c] <- DOC_R[i-1, c] - DOC_R[i, c]
      
      # Update mobile pool and flush out DOC
      DOC_mobile[i, c] <-  (DOC_mobile[i-1, c] + DOC_R_moving[i,c]) * lambda_DOC[exp_type_code[c]]
      
      # Porewater is the mobile pool
      DOC_pw[i, c] <- DOC_mobile[i, c] + DOC_inf
    
      DOC_obs[i, c] ~ dlnorm(log(DOC_pw[i, c]), 1 / sigma_DOC^2)
    
    } # end loop i, rows
    
  } # end loop c, concentrations
  
} # end model
"

library(rjags)

inits_1 <- list(lambda_Ca = c(0.5, 0.9),lambda_cond = c(0.4, 0.5), lambda_DOC = c(0.1, 0.6),
                Ca_inf_ASW = 0.5, 
                sigma_Ca = 0.2, sigma_cond = 0.1, Ca_b = -4, 
                Ca_a = 0.7, cond_a=0.5, cond_b = 1,
                DOC_inf = 7, DOC_init = 5,
                DOC_a = 0.001, DOC_b = 1, DOC_c = 0,
                DOC_max = 50, k_m = 100
                )
inits_2 <- list(lambda_Ca = c(0.6, 0.8), lambda_cond = c(0.2, 0.6), lambda_DOC = c(0.1, 0.6),
                Ca_inf_ASW = 0.8, 
                sigma_Ca = 0.3, sigma_cond = 0.5, Ca_b = -3, 
                Ca_a = 0.5,cond_a=0.1, cond_b = 5,
                DOC_inf = 7, DOC_init = 5,
                DOC_a = 0.002, DOC_b = 0.7, DOC_c = 1,
                DOC_max = 60, k_m = 90
                )
inits_3 <- list(lambda_Ca = c(0.1, 0.7), lambda_cond = c(0.2, 0.7), lambda_DOC = c(0.1, 0.6),
                Ca_inf_ASW = 0.3, 
                sigma_Ca = 0.1, sigma_cond = 0.2, Ca_b = -5, 
                Ca_a = 0.6,cond_a=1, cond_b = 1,
                DOC_inf = 7, DOC_init = 5,
                DOC_a = 0.002, DOC_b = 2, DOC_c = 0,
                DOC_max = 70, k_m = 100
                )
inits_4 <- list(lambda_Ca = c(0.1, 0.5), lambda_cond = c(0.2, 0.6), lambda_DOC = c(0.1, 0.9), 
                Ca_inf_ASW = 0.5, 
                sigma_Ca = 0.4, sigma_cond = 0.4, Ca_b = -4, 
                Ca_a = 0.8, cond_a=0.1, cond_b = 2,
                DOC_inf = 7, DOC_init = 5,
                DOC_a = 0.002, DOC_b = 1, DOC_c = -1,
                DOC_max = 70, k_m = 120
                )

jm <- jags.model(
  file     = textConnection(Ca_flush_model),
  data     = jags_data,
  inits    = list(inits_1, inits_2, inits_3, inits_4),
  n.chains = 4,
  n.adapt  = 2000
)

update(jm, n.iter = 5000)

params <- c("lambda_Ca", "lambda_cond", "lambda_DOC",
            "Ca_inf_ASW", "DOC_inf",
            "Ca_a", "Ca_b", "cond_a", "cond_b", 
            "DOC_max", "k_m",
            "DOC_a", "DOC_b", "DOC_c",
            "cond_init", "DOC_init",
            "sigma_cond", "sigma_DOC","sigma_Ca")

samples <- coda.samples(
  model          = jm,
  variable.names = params,
  n.iter         = 10000,
  thin           = 5
)

library(ggmcmc)

tidy_jags <- ggs(samples)

arrow::write_parquet(tidy_jags, "BHM/output/tidy_jags.parquet")

ggplot(tidy_jags, aes(x = value)) +
  geom_density(aes(fill = as.character(Chain)), alpha = 0.5) +
  facet_wrap(.~Parameter, scale = "free")

ggplot(tidy_jags, aes(x = Iteration, y = value)) +
  geom_line(aes(color = as.character(Chain))) +
  facet_wrap(.~Parameter, scale = "free")

subset_jags <- tidy_jags %>% 
  pivot_wider(names_from = Parameter, 
              values_from = value
  ) %>% 
  slice_sample(n = 100)

# Inputs
library(dplyr)
library(tidyr)

# =========================================================
# Inputs
# =========================================================

unique_simulations <- treatment_solutions %>% 
  ungroup() %>% 
  select(-c(Column, treatment_id)) %>% 
  distinct_all() %>% 
  mutate(simulation_id = 1:n())
  

Ca_flood    <- unique_simulations$Ca_ppm
cond_flood  <- unique_simulations$conductivity_uSpercm
is_nacl     <- unique_simulations$is_nacl
exp_type_code <- as.integer(unique_simulations$exp_type_code)
n_experiments <- length(Ca_flood)
simulation_id <- unique_simulations$simulation_id

N_obs <- unique_simulations$n

Ca_inf_NaCl <- 0.01

# =========================================================
# Output container
# =========================================================

all_output <- vector("list", nrow(subset_jags))

# =========================================================
# Posterior draw loop
# =========================================================

for (j in 1:nrow(subset_jags)) {
  
  # -------------------------------------------------------
  # Parameters
  # -------------------------------------------------------
  
  # Flush rates now vary by experiment type
  lambda_Ca <- c(
    subset_jags$`lambda_Ca[1]`[j],
    subset_jags$`lambda_Ca[2]`[j]
  )
  
  lambda_cond <- c(
    subset_jags$`lambda_cond[1]`[j],
    subset_jags$`lambda_cond[2]`[j]
  )
  
  lambda_DOC <- c(
    subset_jags$`lambda_DOC[1]`[j],
    subset_jags$`lambda_DOC[2]`[j]
  )
  
  # Ca model
  Ca_a <- subset_jags$Ca_a[j]
  Ca_b <- subset_jags$Ca_b[j]
  
  Ca_inf_ASW <- subset_jags$Ca_inf_ASW[j]
  
  # Conductivity model
  cond_a    <- subset_jags$cond_a[j]
  cond_b    <- subset_jags$cond_b[j]
  cond_init <- subset_jags$cond_init[j]
  
  # DOC model
  DOC_max  <- subset_jags$DOC_max[j]
  k_m      <- subset_jags$k_m[j]
  DOC_init <- subset_jags$DOC_init[j]
  
  DOC_a <- subset_jags$DOC_a[j]
  DOC_b <- subset_jags$DOC_b[j]
  DOC_c <- subset_jags$DOC_c[j]
  
  DOC_inf <- subset_jags$DOC_inf[j]
  
  # -------------------------------------------------------
  # Experiment output
  # -------------------------------------------------------
  
  experiment_output <- vector("list", n_experiments)
  
  # =======================================================
  # Experiment loop
  # =======================================================
  
  for (c in 1:n_experiments) {
    
    n_t <- N_obs[c]
    
    # -----------------------------------------------------
    # Select experiment-specific flush rates
    # -----------------------------------------------------
    
    exp_type <- exp_type_code[c]
    
    lambda_Ca_c   <- lambda_Ca[exp_type]
    lambda_cond_c <- lambda_cond[exp_type]
    lambda_DOC_c  <- lambda_DOC[exp_type]
    
    # -----------------------------------------------------
    # Allocate vectors
    # -----------------------------------------------------
    
    Ca_mobile <- rep(NA_real_, n_t)
    Ca_pw     <- rep(NA_real_, n_t)
    
    cond_mobile <- rep(NA_real_, n_t)
    cond_pw     <- rep(NA_real_, n_t)
    
    DOC_R_logit  <- rep(NA_real_, n_t)
    DOC_R        <- rep(NA_real_, n_t)
    DOC_R_moving <- rep(NA_real_, n_t)
    
    DOC_mobile <- rep(NA_real_, n_t)
    DOC_pw     <- rep(NA_real_, n_t)
    
    # -----------------------------------------------------
    # Initial conditions
    # -----------------------------------------------------
    
    # Calcium
    Ca_init <- exp(
      Ca_a * log(cond_flood[c]) + Ca_b
    )
    
    Ca_inf <- is_nacl[c] * Ca_inf_NaCl +
      (1 - is_nacl[c]) * Ca_inf_ASW
    
    Ca_mobile[1] <- (Ca_flood[c] - Ca_inf) + Ca_init
    
    Ca_pw[1] <- Ca_mobile[1] + Ca_inf
    
    # Conductivity
    cond_inf <- exp(
      cond_a * log(cond_flood[c]) + cond_b
    )
    
    cond_mobile[1] <-
      (cond_flood[c] - cond_inf) + cond_init
    
    cond_pw[1] <- cond_mobile[1] + cond_inf
    
    # DOC
    DOC_pool <- DOC_init +
      (
        DOC_max * cond_flood[c] /
          (k_m + cond_flood[c])
      )
    
    DOC_R_logit[1] <-
      DOC_c +
      DOC_b * Ca_pw[1] +
      DOC_a * cond_pw[1]
    
    DOC_R[1] <-
      DOC_pool /
      (1 + exp(-DOC_R_logit[1]))
    
    DOC_mobile[1] <-
      DOC_pool - DOC_R[1]
    
    DOC_pw[1] <- DOC_mobile[1] + DOC_inf
    
    # =====================================================
    # Time loop
    # =====================================================
    
    if (n_t > 1) {
      
      for (i in 2:n_t) {
        
        # -------------------------------------------------
        # Ca
        # -------------------------------------------------
        
        Ca_mobile[i] <-
          Ca_mobile[i - 1] * lambda_Ca_c
        
        Ca_pw[i] <-
          Ca_mobile[i] + Ca_inf
        
        # -------------------------------------------------
        # Conductivity
        # -------------------------------------------------
        
        cond_mobile[i] <-
          cond_mobile[i - 1] * lambda_cond_c
        
        cond_pw[i] <-
          cond_mobile[i] + cond_inf
        
        # -------------------------------------------------
        # DOC resistant pool
        # -------------------------------------------------
        
        DOC_R_logit[i] <-
          DOC_c +
          DOC_b * Ca_pw[i] +
          DOC_a * cond_pw[i]
        
        DOC_R[i] <-
          DOC_pool /
          (1 + exp(-DOC_R_logit[i]))
        
        # -------------------------------------------------
        # DOC transfer
        # -------------------------------------------------
        
        DOC_R_moving[i] <-
          DOC_R[i - 1] - DOC_R[i]
        
        # -------------------------------------------------
        # Mobile DOC
        # -------------------------------------------------
        
        DOC_mobile[i] <-
          (
            DOC_mobile[i - 1] +
              DOC_R_moving[i]
          ) * lambda_DOC_c
        
        DOC_pw[i] <-
          DOC_mobile[i] + DOC_inf
        
      } # end of time loop
      
    } # end n_t > 1?
    
    # =====================================================
    # Convert to tidy format
    # =====================================================
    
    df <- tibble(
      Timepoint = 1:n_t,
      simulation_id = simulation_id[c],
      exp_type_code = exp_type,
      Ca_ppm = Ca_pw,
      conductivity_uSpercm = cond_pw,
      doc_mgperL = DOC_pw
    ) %>%
      pivot_longer(
        cols = c(
          Ca_ppm,
          conductivity_uSpercm,
          doc_mgperL
        ),
        names_to = "analyte",
        values_to = "value"
      )
    
    experiment_output[[c]] <- df
    
  } # end experiment loop
  
  all_output[[j]] <-
    bind_rows(experiment_output) %>%
    mutate(j = j)
  
} # end posterior draw loop

# =========================================================
# Final combined output
# =========================================================

df_long_output <- bind_rows(all_output)

treatment_indexes <- unique_simulations %>% 
  select(Exp_Type, Treatment, simulation_id)

df_long_plot <- df_long_output %>% 
  group_by(simulation_id, Timepoint, analyte) %>% 
  summarise(upper_ci = quantile(value, 0.9),
            lower_ci = quantile(value, 0.01),
            value = median(value)) %>% 
  # mutate(treatment_id = as.integer(treatment_id)) %>% 
  left_join(treatment_indexes)

all_experiments_simplest_long <- all_experiments_simplest %>% 
  select(Exp_Type, Treatment, Column, treatment_id, conductivity_uSpercm, Ca_ppm, doc_mgperL, Timepoint) %>% 
  pivot_longer(values_to = "value",
               names_to = "analyte",
               -c(Exp_Type, Treatment, treatment_id, Timepoint, Column))

write_csv(all_experiments_simplest_long, "BHM/output/all_simple_long.csv")

ggplot(df_long_plot, aes(x = Timepoint, y = value, color = Treatment)) +
  geom_point(data = all_experiments_simplest_long) +
  geom_line() +
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci, fill = Treatment), alpha=0.3) +
  facet_grid(analyte~Exp_Type, scale = "free_y") +
  scale_y_log10()

ggplot(df_long_plot, aes(x = Timepoint, y = value, color = Treatment)) +
  geom_point(data = all_experiments_simplest_long) +
  geom_line() +
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci, fill = Treatment), alpha=0.3) +
  facet_grid(analyte~Exp_Type, scale = "free_y")

ggplot(df_long_plot, aes(x = Timepoint, y = value, color = Exp_Type)) +
  geom_point(data = all_experiments_simplest_long) +
  geom_line() +
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci, fill = Exp_Type), alpha=0.3) +
  facet_grid(analyte~Treatment, scale = "free_y") +
  scale_y_log10()
  

ggplot(df_long_plot, aes(x = Timepoint, y = value, color = Exp_Type)) +
  geom_point(data = all_experiments_simplest_long) +
  geom_line() +
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci, fill = Exp_Type), alpha=0.3) +
  facet_grid(analyte~Treatment, scale = "free_y") +
  scale_y_log10()

write_csv(df_long_plot, "BHM/output/experiment_predictive_checks.csv")

# Save some outputs


