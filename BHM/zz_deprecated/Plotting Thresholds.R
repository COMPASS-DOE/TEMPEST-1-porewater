# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# 1. Compute posterior threshold distributions (analytical — no model runs)
# -----------------------------------------------------------------------------

thresholds <- subset_jags %>%
  select(DOC_a, DOC_b, DOC_c) %>%
  mutate(
    cond_threshold = -DOC_c / DOC_a,   # vdW process only
    Ca_threshold   = -DOC_c / DOC_b    # cation bridging only
  )

# Summary table
threshold_summary <- thresholds %>%
  summarise(across(
    c(cond_threshold, Ca_threshold),
    list(
      Q025 = ~quantile(., 0.025),
      Q10  = ~quantile(., 0.10),
      Q25  = ~quantile(., 0.25),
      Q50  = ~quantile(., 0.50),
      Q75  = ~quantile(., 0.75),
      Q90  = ~quantile(., 0.90),
      Q975 = ~quantile(., 0.975)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "quantile"),
               names_sep = "__") %>%
  pivot_wider(names_from = quantile, values_from = value)

print(threshold_summary)
write.csv(threshold_summary, "threshold_summary.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 2. Build sigmoid curves for each mechanism
#    Each draw from the posterior gives a different sigmoid shape + threshold
# -----------------------------------------------------------------------------

N_CURVES <- 300   # posterior draws to show as background lines

cond_seq <- seq(0, 13240, length.out = 400)   # adjust range to your system
Ca_seq   <- seq(0, 65,   length.out = 400)

posterior_sample <- thresholds %>% slice_sample(n = N_CURVES)

# vdW sigmoid (DOC_b = 0)
vdw_curves <- posterior_sample %>%
  rowwise() %>%
  mutate(
    x         = list(cond_seq),
    retention = list(1 / (1 + exp(-(DOC_c + DOC_a * cond_seq))))
  ) %>%
  unnest(c(x, retention)) %>%
  mutate(mechanism = "van der Waals\n(conductivity)")

# Cation bridging sigmoid (DOC_a = 0)
cb_curves <- posterior_sample %>%
  rowwise() %>%
  mutate(
    x         = list(Ca_seq),
    retention = list(1 / (1 + exp(-(DOC_c + DOC_b * Ca_seq))))
  ) %>%
  unnest(c(x, retention)) %>%
  mutate(mechanism = "Cation bridging\n(calcium)")

# Median curves
med <- as.list(medians_row)

median_vdw <- tibble(
  x         = cond_seq,
  retention = 1 / (1 + exp(-(med$DOC_c + med$DOC_a * cond_seq))),
  mechanism = "van der Waals\n(conductivity)"
)

median_cb <- tibble(
  x         = Ca_seq,
  retention = 1 / (1 + exp(-(med$DOC_c + med$DOC_b * Ca_seq))),
  mechanism = "Cation bridging\n(calcium)"
)

# Threshold quantiles for annotation
vdw_q  <- quantile(thresholds$cond_threshold, c(0.025, 0.50, 0.975))
cb_q   <- quantile(thresholds$Ca_threshold,   c(0.025, 0.50, 0.975))

# -----------------------------------------------------------------------------
# 3. Plot A — vdW (conductivity) sigmoid
# -----------------------------------------------------------------------------

p_vdw <- ggplot() +
  geom_line(
    data = vdw_curves,
    aes(x = x, y = retention,
        group = interaction(DOC_a, DOC_b, DOC_c)),
    colour = "steelblue", alpha = 0.04
  ) +
  geom_line(
    data = median_vdw,
    aes(x = x, y = retention),
    colour = "steelblue4", linewidth = 1
  ) +
  # 95% CI on threshold
  annotate("rect",
           xmin = vdw_q[1], xmax = vdw_q[3],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick", alpha = 0.08) +
  geom_vline(xintercept = vdw_q[2],
             linetype = "dashed", colour = "firebrick", linewidth = 0.8) +
  geom_hline(yintercept = 0.5,
             linetype = "dotted", colour = "grey40") +
  annotate("text",
           x = vdw_q[2], y = 0.05,
           label = paste0("Threshold\n", round(vdw_q[2], 0), " µS/cm\n[",
                          round(vdw_q[1], 0), "–", round(vdw_q[3], 0), "]"),
           hjust = -0.1, size = 3, colour = "firebrick") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = "van der Waals (conductivity)",
    subtitle = "Cation bridging coefficient zeroed out",
    x        = "Porewater conductivity (µS/cm)",
    y        = "Fraction of DOC pool retained"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# -----------------------------------------------------------------------------
# 4. Plot B — Cation bridging (calcium) sigmoid
# -----------------------------------------------------------------------------

p_cb <- ggplot() +
  geom_line(
    data = cb_curves,
    aes(x = x, y = retention,
        group = interaction(DOC_a, DOC_b, DOC_c)),
    colour = "darkorange", alpha = 0.04
  ) +
  geom_line(
    data = median_cb,
    aes(x = x, y = retention),
    colour = "darkorange4", linewidth = 1
  ) +
  annotate("rect",
           xmin = cb_q[1], xmax = cb_q[3],
           ymin = -Inf, ymax = Inf,
           fill = "firebrick", alpha = 0.08) +
  geom_vline(xintercept = cb_q[2],
             linetype = "dashed", colour = "firebrick", linewidth = 0.8) +
  geom_hline(yintercept = 0.5,
             linetype = "dotted", colour = "grey40") +
  annotate("text",
           x = cb_q[2], y = 0.05,
           label = paste0("Threshold\n", round(cb_q[2], 1), " mg/L\n[",
                          round(cb_q[1], 1), "–", round(cb_q[3], 1), "]"),
           hjust = -0.1, size = 3, colour = "firebrick") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  labs(
    title    = "Cation bridging (calcium)",
    subtitle = "van der Waals coefficient zeroed out",
    x        = expression(Ca[pw]~(mg~L^{-1})),
    y        = "Fraction of DOC pool retained"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# -----------------------------------------------------------------------------
# 5. Plot C — posterior density of both thresholds (normalised x-axis)
#    Puts both mechanisms on one panel by expressing threshold as a
#    percentile of the observed flood range, so units are comparable.
#    Adjust COND_FLOOD and CA_FLOOD to your flood values.
# -----------------------------------------------------------------------------

thresholds_norm <- thresholds %>%
  mutate(
    cond_pct = cond_threshold / COND_FLOOD * 100,
    Ca_pct   = Ca_threshold   / CA_FLOOD   * 100
  ) %>%
  select(cond_pct, Ca_pct) %>%
  pivot_longer(everything(),
               names_to  = "mechanism",
               values_to = "threshold_pct") %>%
  mutate(mechanism = recode(mechanism,
                            "cond_pct" = "van der Waals (conductivity)",
                            "Ca_pct"   = "Cation bridging (calcium)"
  ))

p_density <- thresholds_norm %>%
  ggplot(aes(x = threshold_pct, fill = mechanism, colour = mechanism)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +
  geom_vline(
    data = thresholds_norm %>%
      group_by(mechanism) %>%
      summarise(med = median(threshold_pct)),
    aes(xintercept = med, colour = mechanism),
    linetype = "dashed", linewidth = 0.8
  ) +
  scale_fill_manual(values  = c("van der Waals (conductivity)" = "steelblue",
                                "Cation bridging (calcium)"    = "darkorange")) +
  scale_colour_manual(values = c("van der Waals (conductivity)" = "steelblue4",
                                 "Cation bridging (calcium)"    = "darkorange4")) +
  geom_vline(xintercept = 100, linetype = "dotted", colour = "grey40") +
  annotate("text", x = 102, y = Inf, label = "Flood value",
           hjust = 0, vjust = 1.5, size = 3, colour = "grey40") +
  labs(
    title    = "Threshold Uncertainty: Both Mechanisms",
    subtitle = "Threshold expressed as % of flood concentration; dashed = posterior median",
    x        = "Threshold (% of flood value)",
    y        = "Posterior density",
    fill     = NULL, colour = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor  = element_blank(),
        legend.position   = "bottom")

# -----------------------------------------------------------------------------
# 6. Combine and save
# -----------------------------------------------------------------------------

p_combined <- (p_vdw | p_cb) / p_density +
  plot_annotation(
    title    = "DOC Retention Thresholds: Independent Process Analysis",
    subtitle = "Each mechanism isolated by zeroing the non-focal coefficient",
    theme    = theme(plot.title    = element_text(face = "bold"),
                     plot.subtitle = element_text(colour = "grey40"))
  )
(p_combined)
