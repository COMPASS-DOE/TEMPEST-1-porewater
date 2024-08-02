anova

timedate_stats <- function(df, var, plot, p_value_threshold) {
  library(dplyr)
  library(rlang)
  library(car)        # For Levene's Test and Box-Cox transformation
  library(ggplot2)    # For diagnostic plots
  library(broom)      # For tidy model outputs
  
  # Convert var to quosure
  var <- enquo(var)
  
  # Step 1: Divide data and mutate
  data_stats <- df %>%
    divide_data(datetime_est, flood_event_datetime, rain_event_datetime, "Rain") %>%
    mutate(timedate_bin = factor(timedate_bin, levels = c("Pre", "Mid", "Btwn Events", "Rain", "Post"))) %>%
    group_by(timedate_bin, plot)
  
  # Convert the plot to a character string to match with the values in the dataframe
  plot <- as.character(ensym(plot))
  
  # Debugging: print the unique plot values in the dataset
  cat("### Plot chosen:\n")
  print(unique(plot))
  
  # Ensuring the plot values exist in the data
  if (!(plot %in% unique(data_stats$plot))) {
    stop("The specified plot does not exist in the dataset.")
  }
  
  # Filter the data
  data_filtered <- data_stats %>% filter(plot == !!plot)
  
  # Print the filtered data summary grouped by plot for verification
  cat("### Filtered data summary for plot:", plot, "\n")
  print(data_filtered %>% group_by(plot) %>% tally())
  
  data_summary <- data_filtered %>%
    group_by(plot) %>%
    summarize(across(where(is.numeric), list(mean = mean, sd = sd, n = ~sum(!is.na(.))), .names = "summary_{col}_{fn}")) %>%
    ungroup()
  
  cat("### Summary of filtered data grouped by plot:\n")
  print(data_summary)
  
  # Function to try different transformations
  try_transformations <- function(data, var_name) {
    transform_success <- function(data, var_name) {
      formula <- as.formula(paste(var_name, "~ timedate_bin"))
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
  formula <- as.formula(paste(quo_name(var), "~ timedate_bin"))
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
  
  # Step 6: Check for significant main effect of time
  time_term <- "timedate_bin"
  time_p_value <- ifelse(time_term %in% row.names(model_summary[[1]]), model_summary[[1]][time_term, "Pr(>F)"], NA)
  
  significant_time <- !is.na(time_p_value) && time_p_value < p_value_threshold
  
  # Initialize Tukey result container
  significant_timedate_bin <- NULL
  
  # Step 7: Perform Tukey HSD post-hoc tests and print significant results
  if (significant_time) {
    tukey_results <- TukeyHSD(aov_model, conf.level = 0.95)
    tukey_timedate_bin_df <- as.data.frame(tukey_results$`timedate_bin`)
    significant_timedate_bin <- tukey_timedate_bin_df %>% filter(`p adj` < p_value_threshold)
    
    if (nrow(significant_timedate_bin) > 0) {
      cat("### Significant timedate_bin Tukey HSD results with p-value <", p_value_threshold, ":\n")
      print(significant_timedate_bin)
    } else {
      cat("No significant post-hoc differences found for 'timedate_bin'.\n")
    }
  } else {
    cat("No significant main effect found for 'timedate_bin'.\n")
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
    significant_timedate_bin = significant_timedate_bin
  ))
}

# Example Usage
# results <- baci_stats(df = pw_doc_bgc, var = doc_mg_l, plot = "Plot1", p_value_threshold = 0.05)
# results$data_summary
# results$model_summary
# results$shapiro_test
# results$levene_test
# results$significant_timedate_bin