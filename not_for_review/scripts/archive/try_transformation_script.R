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
