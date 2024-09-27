# BACI Stats Function
# Following some AI Incubator conversations: 
#   
#   Need to focus on the treatment plots one at a time. 
# 
# "The main statistic of interest in a BACI analysis is the interaction term (Before-After × Control-Impact), which would be significant when a change occurs at the impact site but not at the control site."
# "Restricted maximum likelihood estimation (REML) ANOVA, which is considered more robust for unbalanced designs"
# https://www.facetsjournal.com/doi/full/10.1139/facets-2016-0058 
# 
# Main Effects: Check the significance of Time and Treatment main effects.
# Interaction: Focus on the interaction term (Time:Treatment interaction) to understand if the impact event had a different effect over time between the Control and Impact groups.
# 
## Full BACI stats Code

#r stats function for the paper

source("~/GitHub/tempest-system-level-analysis/scripts/tmp_test_functions.R")

baci_stats <- function(data_stats, var, plot, p_value_threshold) {
browser()
  library(dplyr)
  library(rlang)
  library(car)
  library(ggplot2)
  library(broom)
  
  var <- enquo(var)
  
  # Debugging: Print the data received
  cat("Data received in baci_stats:\n")
  print(head(data_stats))
  
  # Ensure timedate_bin is a factor with correct levels
  data_stats <- data_stats %>%
    mutate(timedate_bin = factor(timedate_bin, levels = c("Pre", "Mid", "Post")))
  
  # Debugging: Print unique plot values before filtering
  cat("Unique plot values in data_stats before filtering:\n")
  print(unique(data_stats$plot))
  
  # Convert the plot to a character string to match with the values in the dataframe
  plot <- as.character(ensym(plot))
  control_plot <- "Control"
  
  plots_chosen <- c(plot, control_plot)
  
  # Debugging: Print the chosen plots
  cat("### Plots chosen:\n")
  print(plots_chosen)
  
  # Ensuring the plot values exist in the data
  if (!(plot %in% unique(data_stats$plot)) | !(control_plot %in% unique(data_stats$plot))) {
    stop("One or more specified plots do not exist in the dataset.")
  }
  
  # Filter the data to include chosen plots only
  data_filtered <- data_stats %>% filter(plot %in% plots_chosen)
  
  # Convert plot to factor and ensure it has the correct levels
  data_filtered <- data_filtered %>%
    mutate(plot = factor(plot, levels = plots_chosen))
  
  # Debugging: Confirm levels of plot
  cat("### Levels of plot after conversion to factor:\n")
  print(levels(data_filtered$plot))
  
  # Check for NAs in the variable
  cat("### Check for NAs in the variable after filtering:\n")
  num_NA <- sum(is.na(data_filtered %>% pull(!!var)))
  print(num_NA)
  
  if (num_NA > 0) {
    cat("\nWarning: NA values found in the variable. Removing rows with NA values.\n")
    # Remove NAs
    data_filtered <- data_filtered %>% filter(!is.na(!!var))
  }
  
  # Check the levels of timedate_bin before ANOVA
  cat("### Levels of timedate_bin after filtering:\n")
  print(levels(data_filtered$timedate_bin))
  
  # Debugging: Print the filtered data summary grouped by plot for verification
  cat("### Filtered data summary including 'Control' and chosen plot:", plot, "\n")
  print(data_filtered %>% group_by(plot) %>% tally())
  
  # Verify that both timedate_bin and plot have more than one level
  if (nlevels(data_filtered$plot) < 2) {
    stop("plot has less than 2 levels after filtering. Cannot perform ANOVA.")
  }
  if (nlevels(data_filtered$timedate_bin) < 2) {
    stop("timedate_bin has less than 2 levels after filtering. Cannot perform ANOVA.")
  }
  
  # Summarize data
  data_summary <- data_filtered %>%
    group_by(plot) %>%
    summarize(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                             sd = ~sd(.x, na.rm = TRUE), 
                                             n = ~sum(!is.na(.x))), .names = "summary_{col}_{fn}")) %>%
    ungroup()
  
  cat("### Summary of filtered data grouped by plot:\n")
  print(data_summary)
  
  # Function to try different transformations
  try_transformations <- function(data, var_name) {
    transform_success <- function(data, var_name) {
      formula <- as.formula(paste(var_name, "~ timedate_bin * plot"))
      aov_model <- aov(formula, data = data)
      shapiro_test <- shapiro.test(residuals(aov_model))
      list(success = shapiro_test$p.value >= 0.05, aov_model = aov_model, p_value = shapiro_test$p.value)
    }
    
    # Original variable
    var_name <- quo_name(var)
    
    transformations <- list(
      "Original" = data[[var_name]],
      "log" = log(data[[var_name]] + 1),
      "sqrt" = sqrt(data[[var_name]]),
      "rank" = rank(data[[var_name]]),
      "arcsine_sqrt" = asin(sqrt(data[[var_name]])),
      "boxcox" = {
        bc <- car::powerTransform(data[[var_name]] + 1)
        (data[[var_name]] + 1)^bc$lambda
      }
    )
    
    for (trans_name in names(transformations)) {
      data[[var_name]] <- transformations[[trans_name]]
      result <- transform_success(data, var_name)
      if (result$success) {
        cat(paste("Transformation with", trans_name, "achieved normality.\n"))
        return(result$aov_model)
      } else {
        cat(paste("Transformation with", trans_name, "did not achieve normality (p-value =", result$p_value, ").\n"))
      }
    }
    
    cat("No transformation achieved normality.\n")
    return(aov_model)
  }
  
  # Step 3: Perform ANOVA
  formula <- as.formula(paste(quo_name(var), "~ timedate_bin * plot"))
  aov_model <- aov(formula, data = data_filtered)
  model_residuals <- residuals(aov_model)
  
  # Step 4: Check for ANOVA assumptions
  
  # 1. Normality of Residuals
  cat("### Checking normality of residuals:\n")
  shapiro_test <- shapiro.test(model_residuals)
  print(shapiro_test)
  
  if (shapiro_test$p.value < 0.05) {
    cat("Residuals do not appear to be normally distributed. Attempting transformations...\n")
    aov_model <- try_transformations(data_filtered, var)
    model_residuals <- residuals(aov_model)
    shapiro_test <- shapiro.test(model_residuals)
  } else {
    cat("Residuals appear to be normally distributed.\n")
  }
  
  # 2. Homogeneity of variances (Levene's Test)
  cat("### Checking homogeneity of variances:\n")
  levene_test <- leveneTest(formula, data = data_filtered)
  print(levene_test)
  if (levene_test$`Pr(>F)`[1] >= 0.05) {
    cat("Variances appear to be homogeneous (Levene's Test is not significant).\n")
  } else {
    cat("Variances do not appear to be homogeneous (Levene's Test is significant).\n")
  }
  
  # Step 5: Summary of the ANOVA model
  cat("### ANOVA model summary:\n")
  model_summary <- summary(aov_model)
  print(model_summary)
  
  # Step 6: Check for significant interaction or main effect of time
  interaction_term <- "timedate_bin:plot"
  time_term <- "timedate_bin"
  
  interaction_p_value <- ifelse(interaction_term %in% row.names(model_summary[[1]]), model_summary[[1]][interaction_term, "Pr(>F)"], NA)
  time_p_value <- ifelse(time_term %in% row.names(model_summary[[1]]), model_summary[[1]][time_term, "Pr(>F)"], NA)
  
  significant_interaction <- !is.na(interaction_p_value) && interaction_p_value < p_value_threshold
  significant_time <- !is.na(time_p_value) && time_p_value < p_value_threshold
  
  # Initialize Tukey result containers
  tukey_results <- NULL
  significant_tukey_interaction <- NULL
  significant_timedate_bin <- NULL
  
  # Step 7: Perform Tukey HSD post-hoc tests and print significant results
  if (significant_interaction | significant_time) {
    tukey_results <- TukeyHSD(aov_model, conf.level = 0.95)
    
    # Convert Tukey HSD results to data frames for easier filtering
    tukey_interaction_df <- as.data.frame(tukey_results$`timedate_bin:plot`)
    tukey_timedate_bin_df <- as.data.frame(tukey_results$`timedate_bin`)
    
    significant_tukey_interaction <- tukey_interaction_df %>% filter(`p adj` < p_value_threshold)
    significant_timedate_bin <- tukey_timedate_bin_df %>% filter(`p adj` < p_value_threshold)
    
    # Print significant results
    any_significant <- FALSE  # A flag to track if any significant results are found
    
    if (significant_time) {
      cat("\n### Significant main effect found for 'timedate_bin'.\n")
      if (nrow(significant_timedate_bin) > 0) {
        cat("### Significant timedate_bin Tukey HSD results with p-value <", p_value_threshold, ":\n")
        print(significant_timedate_bin)
      }
      any_significant <- TRUE
    }
    
    if (significant_interaction) {
      cat("\n### Significant interaction found between 'timedate_bin' and 'plot'.\n")
      if (nrow(significant_tukey_interaction) > 0) {
        cat("### Significant interaction Tukey HSD results with p-value <", p_value_threshold, ":\n")
        print(significant_tukey_interaction)
      }
      any_significant <- TRUE
    }
    
    if (!any_significant) {
      cat("No significant interaction or main effect found for 'timedate_bin'.\n")
    }
  }
  
  # Return results as a list
  return(list(
    data_stats = data_stats,
    data_filtered = data_filtered,
    data_summary = data_summary,
    aov_model = aov_model,
    model_summary = model_summary,
    shapiro_test = shapiro_test,
    levene_test = levene_test,
    significant_tukey_interaction = significant_tukey_interaction,
    significant_timedate_bin = significant_timedate_bin
  ))
}

#results <- baci_stats(data_divided, some_var, plot, 0.05)
