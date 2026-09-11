#
# Method: Fix all parameters at median, replace one group at a time with
# full posterior samples. Summarise output spread using IQR (robust to skew).
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(arrow)

tidy_jags <- read_parquet("BHM/output/tidy_jags.parquet")

subset_jags <- tidy_jags %>% 
  pivot_wider(names_from = Parameter, 
              values_from = value
  )

runModelFromTab <- function(input_tab, cond_flood=13240, Ca_flood=43.8, is_nacl = 0,
                            Ca_inf_NaCl = 0.01) {
  
  # Flush rates now vary by experiment type
  lambda_Ca <- c(
    input_tab$`lambda_Ca[1]`[1],
    input_tab$`lambda_Ca[2]`[1]
  )
  
  lambda_cond <- c(
    input_tab$`lambda_cond[1]`[1],
    input_tab$`lambda_cond[2]`[1]
  )
  
  lambda_DOC <- c(
    input_tab$`lambda_DOC[1]`[1],
    input_tab$`lambda_DOC[2]`[1]
  )
  
  # Ca model
  Ca_a <- input_tab$Ca_a[1]
  Ca_b <- input_tab$Ca_b[1]
  
  Ca_inf_ASW <- input_tab$Ca_inf_ASW[1]
  
  # Conductivity model
  cond_a    <- input_tab$cond_a[1]
  cond_b    <- input_tab$cond_b[1]
  cond_init <- input_tab$cond_init[1]
  
  # DOC model
  DOC_max  <- input_tab$DOC_max[1]
  k_m      <- input_tab$k_m[1]
  DOC_init <- input_tab$DOC_init[1]
  
  DOC_a <- input_tab$DOC_a[1]
  DOC_b <- input_tab$DOC_b[1]
  DOC_c <- input_tab$DOC_c[1]
  
  DOC_inf <- input_tab$DOC_inf[1]
  
  n_t <- 12
  
  # -----------------------------------------------------
  # Select experiment-specific flush rates
  # -----------------------------------------------------
  
  exp_type <- 2
  
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
    Ca_a * log(cond_flood) + Ca_b
  )
  
  Ca_inf <- is_nacl * Ca_inf_NaCl +
    (1 - is_nacl) * Ca_inf_ASW
  
  Ca_mobile[1] <- (Ca_flood - Ca_inf) + Ca_init
  
  Ca_pw[1] <- Ca_mobile[1] + Ca_inf
  
  # Conductivity
  cond_inf <- exp(
    cond_a * log(cond_flood) + cond_b
  )
  
  cond_mobile[1] <-
    (cond_flood - cond_inf) + cond_init
  
  cond_pw[1] <- cond_mobile[1] + cond_inf
  
  # DOC
  DOC_pool <- DOC_init +
    (
      DOC_max * cond_flood /
        (k_m + cond_flood)
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
      
    }
  }
  
  
  return(data.frame(Timepoint = 1:12, DOC_pw = DOC_pw, 
                    Ca_pw = Ca_pw, cond_pw = cond_pw))
  
}


# -----------------------------------------------------------------------------
# 0. Parameter group definitions
#    Edit these to reflect your scientific groupings.
# -----------------------------------------------------------------------------

param_groups <- list(
  "Electrostatic Dispersion"      = c("DOC_max", "k_m"),
  "van der Waals"                  = c("DOC_a"),
  "Cation Bridging"                = c("DOC_b"),
  "Other" =
    c("lambda_Ca[1]", "lambda_Ca[2]",
      "lambda_cond[1]", "lambda_cond[2]",
      "lambda_DOC[1]", "lambda_DOC[2]",
      "Ca_a", "Ca_b", "Ca_inf_ASW",
      "cond_a", "cond_b", "cond_init",
      "DOC_c", "DOC_inf", "DOC_init")
)

# How many posterior draws to use per group (trade-off: speed vs smoothness)
N_DRAWS <- 500

# Flood conditions (match your experiment)
COND_FLOOD <- 13240
CA_FLOOD   <- 43.8
IS_NACL    <- 0

# -----------------------------------------------------------------------------
# 1. Build a table of medians (one row, all parameters)
# -----------------------------------------------------------------------------

# `tidy_jags` must be in your environment: columns = Parameter, value, Chain, Iteration
# `subset_jags`  = filtered posterior draws (e.g. thinned, burned-in)

all_params <- tidy_jags %>%
  group_by(Parameter) %>%
  summarise(median_val = median(value), .groups = "drop") %>%
  filter(!Parameter %in% c("sigma_Ca", "sigma_cond", "sigma_DOC"))

medians_row <- as.data.frame(t(all_params$median_val))
names(medians_row) <- all_params$Parameter

# -----------------------------------------------------------------------------
# 2. Helper: run N_DRAWS with one group free, rest fixed at median
# -----------------------------------------------------------------------------

run_group_sensitivity <- function(free_params, group_label,
                                  n_draws = N_DRAWS,
                                  medians = medians_row) {
  
  # Sample rows from the posterior for the free parameters only
  draws_idx <- sample(nrow(subset_jags), n_draws, replace = TRUE)
  free_draws <- subset_jags[draws_idx, free_params, drop = FALSE]
  
  # Build input table: free cols from posterior, rest from medians
  fixed_cols <- medians[, !names(medians) %in% free_params, drop = FALSE]
  
  input_df <- bind_cols(
    free_draws,
    fixed_cols[rep(1, n_draws), , drop = FALSE]
  )
  
  # Run the forward model for each draw
  results <- map_dfr(seq_len(n_draws), function(i) {
    runModelFromTab(
      input_df[i, ],
      cond_flood = COND_FLOOD,
      Ca_flood   = CA_FLOOD,
      is_nacl    = IS_NACL
    ) %>%
      mutate(draw = i, group = group_label)
  })
  
  results
}

# -----------------------------------------------------------------------------
# 3. Also run the "all fixed" baseline (pure median prediction)
# -----------------------------------------------------------------------------

baseline <- runModelFromTab(
  medians_row,
  cond_flood = COND_FLOOD,
  Ca_flood   = CA_FLOOD,
  is_nacl    = IS_NACL
) %>%
  mutate(draw = 1, group = "Baseline\n(all medians)")

# -----------------------------------------------------------------------------
# 4. Run sensitivity for each group
# -----------------------------------------------------------------------------

sensitivity_raw <- imap_dfr(param_groups, function(free_params, label) {
  message("Running group: ", label)
  run_group_sensitivity(free_params, label)
})

all_sensitivity <- bind_rows(sensitivity_raw, baseline) %>% 
  filter(group != "Other")

# -----------------------------------------------------------------------------
# 5. Summarise: IQR and quantile ribbon per group × timepoint
# -----------------------------------------------------------------------------

sensitivity_summary <- all_sensitivity %>%
  group_by(group, Timepoint) %>%
  summarise(
    Q10  = quantile(DOC_pw, 0.10),
    Q25  = quantile(DOC_pw, 0.25),
    Q50  = quantile(DOC_pw, 0.50),
    Q75  = quantile(DOC_pw, 0.75),
    Q90  = quantile(DOC_pw, 0.90),
    IQR  = IQR(DOC_pw),
    var = var(DOC_pw),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 6. Importance metric: area under IQR curve (sum across timepoints)
#    Normalise relative to the most influential group.
# -----------------------------------------------------------------------------

importance <- sensitivity_summary %>%
  filter(group != "Baseline\n(all medians)") %>%
  group_by(group) %>%
  summarise(
    total_var     = sum(var),          # summed IQR across timepoints
    mean_var      = mean(var),
    peak_var      = max(var),
    peak_timepoint = Timepoint[which.max(var)],
    .groups = "drop"
  ) %>%
  mutate(
    rel_importance = sqrt(total_var) / sqrt(max(total_var)) * 100
  ) %>%
  arrange(desc(rel_importance))

# -----------------------------------------------------------------------------
# 7. Plots
# -----------------------------------------------------------------------------

# Colour palette: one colour per group (colourblind-friendly)
group_levels  <- importance$group  # ordered by importance
group_colours <- setNames(
  RColorBrewer::brewer.pal(min(length(group_levels), 8), "Dark2"),
  group_levels
)

sensitivity_summary <- sensitivity_summary %>%
  mutate(group = factor(group, levels = c(group_levels, "Baseline\n(all medians)")))

write_csv(sensitivity_summary, "BHM/output/sensitivity_summary.csv")

importance <- importance %>%
  mutate(group = factor(group, levels = group_levels))

write_csv(importance, "BHM/output/importance.csv")


# -- 7a. Ribbon plot: DOC envelope over time, faceted by parameter group ------

p_ribbon <- sensitivity_summary %>%
  filter(group != "Baseline\n(all medians)") %>%
  ggplot(aes(x = Timepoint)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = group), alpha = 0.25) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = group), alpha = 0.40) +
  geom_line(aes(y = Q50, colour = group), linewidth = 0.8) +
  # Overlay baseline median
  geom_line(
    data = sensitivity_summary %>% filter(group == "Baseline\n(all medians)"),
    aes(y = Q50), colour = "grey30", linetype = "dashed", linewidth = 0.6
  ) +
  facet_wrap(~ group, ncol = 3) +
  scale_fill_manual(values = group_colours, guide = "none") +
  scale_colour_manual(values = group_colours, guide = "none") +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title    = "Posterior Predictive Influence by Parameter Group",
    subtitle = "10–90% (light) and 25–75% (dark)",
    x        = "Timepoint",
    y        = expression(DOC[pw]~(mg~L^{-1}))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )
(p_ribbon)
ggsave("BHM/output/Ribbon Plot.jpg", width = 8.5, height = 5)

# -- 7b. Importance bar chart (total IQR, normalised) -------------------------

p_importance1 <- importance %>%
  ggplot(aes(x = reorder(group, rel_importance), y = rel_importance,
             fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", rel_importance)),
            hjust = -0.15, size = 3.2) +
  scale_fill_manual(values = group_colours, guide = "none") +
  scale_y_continuous(limits = c(0, 115), expand = c(0, 0)) +
  coord_flip() +
  labs(
   #  subtitle = "Summed IQR of DOC across timepoints, normalised to most influential group",
    x        = NULL,
    y        = "Relative importance (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
(p_importance1)

ggsave("BHM/output/Importance Plot.jpg", width = 5, height=4.25)

p_importance <- importance %>%
  ggplot(aes(x = reorder(group, rel_importance), y = rel_importance
             # fill = group
             )) +
  geom_col(width = 0.7) +
  # geom_text(aes(label = sprintf("%.0f%%", rel_importance)),
  #           hjust = -0.15, size = 3.2) +
  # scale_fill_manual(values = group_colours, guide = "none") +
  # scale_y_continuous(limits = c(0, 115), expand = c(0, 0)) +
  coord_flip() +
  labs(#  subtitle = "Summed IQR of DOC across timepoints, normalised to most influential group",
       x        = NULL,
       y        = "Relative importance (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
(p_importance)

# -- 7c. IQR heatmap: importance × timepoint ----------------------------------
#    Useful for showing WHEN each group matters most.

p_heatmap <- sensitivity_summary %>%
  filter(group != "Baseline\n(all medians)") %>%
  mutate(group = factor(group, levels = rev(group_levels))) %>%
  ggplot(aes(x = Timepoint, y = group, fill = sqrt(var))) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "D",
                       name = expression(sigma~(mg~L^{-1}))) +
  scale_x_continuous(breaks = 1:12, expand = c(0, 0)) +
  labs(
    # subtitle = "IQR of DOC predictions when each group varies freely",
    x        = "Timepoint",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 1))
(p_heatmap)

ggsave("BHM/output/heatmap.jpg", width = 5,
       height = 3)

# -----------------------------------------------------------------------------
# 8. Save outputs
# -----------------------------------------------------------------------------


df_long_plot <- read_csv("BHM/output/experiment_predictive_checks.csv") %>% 
  filter(analyte == "doc_mgperL") %>% 
  mutate(Exp_Type = recode(Exp_Type,
                           "ASW" = "Batch - Artificial saltwater",
                           "ColEx" = "Column experiment",
                           "NaCl" = "Batch - NaCl"),
         Treatment = recode(Treatment, "ASW" = "Artificial saltwater"),
         Treatment = factor(Treatment, levels = c("0", "0.1", "1", "5", "25","100",
                                                  "Control", "Freshwater", "Artificial saltwater"
         ))
  )

all_experiments_simplest_long <- read_csv("BHM/output/all_simple_long.csv") %>% 
  filter(analyte == "doc_mgperL",
         Timepoint > 0) %>% 
  mutate(Exp_Type = recode(Exp_Type,
                           "ASW" = "Batch - Artificial saltwater",
                           "ColEx" = "Column experiment",
                           "NaCl" = "Batch - NaCl"),
         Treatment = recode(Treatment, "ASW" = "Artificial saltwater"),
         Treatment = factor(Treatment, levels = c("0", "0.1", "1", "5", "25","100",
                                                  "Control", "Freshwater", "Artificial saltwater"
                                                  ))
         )

batch_treatments <- RColorBrewer::brewer.pal(6, "YlOrRd")

experiment_model_plot <- ggplot(df_long_plot, aes(x = Timepoint, y = value, color = Treatment)) +
  geom_point(data = all_experiments_simplest_long) +
  geom_line() +
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci, fill = Treatment), alpha=0.3) +
  facet_grid(.~Exp_Type, scale = "free") +
  scale_y_log10() +
  scale_x_continuous(breaks = 1:12) +
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
  theme_minimal() + 
  # theme(legend.position = "bottom",
  #       legend.title = element_blank()) +
  labs(
    x        = "Timepoint",
    y        = expression(DOC[pw]~(mg~L^{-1}))
  )
(experiment_model_plot)
library(gridExtra)

grid.arrange(experiment_model_plot + ggtitle("A. Observations and Model Predictions"),
                                   p_importance + ggtitle("B. Total Parameter\nGroup Importance"), 
                                   p_heatmap + ggtitle("C. Parameter Group\nInfluence Over Time"),
                                   layout_matrix = matrix(c(1,2,1,3), ncol=2)
                                   )

all_together_grobs <- arrangeGrob(experiment_model_plot + ggtitle("A. Observations and Model Predictions"),
                                   p_importance + ggtitle("B. Total Parameter\nGroup Importance"), 
                                   p_heatmap + ggtitle("C. Parameter Group\nInfluence Over Time"),
                                   layout_matrix = matrix(c(1,2,1,3), ncol=2)
)

ggsave("BHM/output/BHM Parameter Importance Figure.jpg",
       width = 8.5, height = 5.5, all_together_grobs)
