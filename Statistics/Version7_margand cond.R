# Library loading - consistent use of tidyverse and related packages
library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models
library(caret)         # For cross-validation
library(knitr)         # For table creation
library(kableExtra)    # For enhanced tables

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Subset data to remove NA values - using tidyverse approach
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# ------ CROSS-VALIDATION APPROACH WITH ESTABLISHED FUNCTIONS ------

# Function to perform k-fold cross-validation for mixed-effects models using caret
cross_validate_mixed_models <- function(data, outcome_var, k=10) {
  # Create a results dataframe
  results <- data.frame(
    Fold = integer(),
    Model = character(),
    Marginal_R2 = numeric(),
    Conditional_R2 = numeric(),
    RMSE = numeric(),
    MAE = numeric()
  )
  
  # Create folds using caret - stratified by participant if possible
  set.seed(123)
  if("Participant" %in% colnames(data)) {
    folds <- createFolds(data$Participant, k = k, returnTrain = TRUE)
  } else {
    folds <- createFolds(1:nrow(data), k = k, returnTrain = TRUE)
  }
  
  # Variables to model
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  # Loop through folds
  for (i in 1:k) {
    # Get training and test data
    train <- data[folds[[i]], ]
    test <- data[-folds[[i]], ]
    
    # Loop through predictors to create and evaluate models
    for (pred in predictors) {
      # Create formula
      formula <- as.formula(paste(outcome_var, "~ Age +", pred, "+ (1|Participant)"))
      
      # Fit model on training data
      model <- lmer(formula, data = train)
      
      # Get predictions on test data
      predictions <- predict(model, newdata = test, allow.new.levels = TRUE)
      
      # Calculate performance metrics
      actual <- test[[outcome_var]]
      rmse <- sqrt(mean((actual - predictions)^2))
      mae <- mean(abs(actual - predictions))
      
      # Calculate R-squared using MuMIn on the training model
      r2_values <- r.squaredGLMM(model)
      marginal_r2 <- r2_values[1]  # Fixed effects only
      conditional_r2 <- r2_values[2]  # Fixed + random effects
      
      # Add to results
      results <- results %>% add_row(
        Fold = i,
        Model = pred,
        Marginal_R2 = marginal_r2,
        Conditional_R2 = conditional_r2,
        RMSE = rmse,
        MAE = mae
      )
    }
  }
  
  return(results)
}

# Perform cross-validation
cat("\nPerforming cross-validation for Sleep Slope...\n")
cv_slope_results <- cross_validate_mixed_models(data_slope, "Sleep_Slope_Matched")

cat("\nPerforming cross-validation for Sleep Amplitude...\n")
cv_amp_results <- cross_validate_mixed_models(data_amp, "Sleep_Amplitude")

# ------ VISUALIZE CROSS-VALIDATION RESULTS WITH SEPARATE PLOTS ------

# Function to create separate boxplots of R-squared values
plot_cv_results <- function(cv_results, title) {
  # Create plot for Marginal R²
  p_marginal <- cv_results %>%
    ggplot(aes(x = Model, y = Marginal_R2, fill = Model)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(
      title = paste(title, "- Marginal R² Values"),
      subtitle = "Fixed Effects Only (Age + Wake Measure)",
      y = "Marginal R²",
      x = "Predictor"
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # Create plot for Conditional R²
  p_conditional <- cv_results %>%
    ggplot(aes(x = Model, y = Conditional_R2, fill = Model)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Greens") +
    labs(
      title = paste(title, "- Conditional R² Values"),
      subtitle = "Fixed + Random Effects (Including Participant)",
      y = "Conditional R²",
      x = "Predictor"
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # Return both plots as a list
  return(list(
    marginal = p_marginal,
    conditional = p_conditional
  ))
}

# Plot results
plot_slope <- plot_cv_results(cv_slope_results, "Sleep Slope")
plot_amp <- plot_cv_results(cv_amp_results, "Sleep Amplitude")

# Display plots
print(plot_slope$marginal)
print(plot_slope$conditional)
print(plot_amp$marginal)
print(plot_amp$conditional)

# ------ SUMMARIZE CROSS-VALIDATION RESULTS ------

# Function to summarize CV results
summarize_cv_results <- function(cv_results, title) {
  cat("\n===== SUMMARY OF", title, "CROSS-VALIDATION RESULTS =====\n")
  
  # Summarize by model
  summary_table <- cv_results %>%
    group_by(Model) %>%
    summarize(
      Mean_Marginal_R2 = mean(Marginal_R2),
      SD_Marginal_R2 = sd(Marginal_R2),
      Mean_Conditional_R2 = mean(Conditional_R2),
      SD_Conditional_R2 = sd(Conditional_R2),
      Mean_RMSE = mean(RMSE),
      Mean_MAE = mean(MAE)
    ) %>%
    arrange(desc(Mean_Conditional_R2))  # Sort by highest conditional R²
  
  # Print table
  print(kable(summary_table, digits = 3, caption = paste(title, "Cross-Validation Results")) %>%
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  # Find best predictor based on conditional R²
  best_predictor <- summary_table$Model[1]
  cat("\nBest predictor (highest conditional R²):", best_predictor, "\n")
  
  return(summary_table)
}

# Summarize results
slope_summary <- summarize_cv_results(cv_slope_results, "SLEEP SLOPE")
amp_summary <- summarize_cv_results(cv_amp_results, "SLEEP AMPLITUDE")

# ------ FIT FINAL MODELS AND COMPARE ------

# Function to fit and compare models
fit_and_compare_models <- function(data, outcome_var, title) {
  cat("\n===== FINAL MODEL COMPARISON FOR", title, "=====\n")
  
  # Variables to model
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  # Create list to store models
  models <- list()
  
  # Fit models
  for (pred in predictors) {
    # Create formula
    formula <- as.formula(paste(outcome_var, "~ Age +", pred, "+ (1|Participant)"))
    
    # Fit model
    models[[pred]] <- lmer(formula, data = data)
  }
  
  # Create comparison table
  comparison_table <- data.frame(
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    Marginal_R2 = numeric(),
    Conditional_R2 = numeric(),
    Fixed_Effect_Estimate = numeric(),
    Fixed_Effect_SE = numeric(),
    Fixed_Effect_p = numeric()
  )
  
  # Populate comparison table
  for (pred in predictors) {
    model <- models[[pred]]
    
    # Get AIC and BIC
    model_aic <- AIC(model)
    model_bic <- BIC(model)
    
    # Get R² values
    r2_values <- r.squaredGLMM(model)
    
    # Get fixed effect estimates
    coefs <- summary(model)$coefficients
    estimate <- coefs[pred, "Estimate"]
    se <- coefs[pred, "Std. Error"]
    p_value <- coefs[pred, "Pr(>|t|)"]
    
    # Add to table
    comparison_table <- comparison_table %>% add_row(
      Model = pred,
      AIC = model_aic,
      BIC = model_bic,
      Marginal_R2 = r2_values[1],
      Conditional_R2 = r2_values[2],
      Fixed_Effect_Estimate = estimate,
      Fixed_Effect_SE = se,
      Fixed_Effect_p = p_value
    )
  }
  
  # Sort by AIC
  comparison_table <- comparison_table %>% arrange(AIC)
  
  # Print table
  print(kable(comparison_table, digits = 3, caption = paste(title, "Model Comparison")) %>%
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  # Print best model
  cat("\nBest model according to AIC:", comparison_table$Model[1], "\n")
  cat("Best model according to BIC:", comparison_table %>% arrange(BIC) %>% pull(Model) %>% first(), "\n")
  
  # Print summary of best model
  cat("\nSummary of best model:\n")
  print(summary(models[[comparison_table$Model[1]]]))
  
  return(list(
    comparison_table = comparison_table,
    models = models
  ))
}

# Fit and compare models
slope_models <- fit_and_compare_models(data_slope, "Sleep_Slope_Matched", "SLEEP SLOPE")
amp_models <- fit_and_compare_models(data_amp, "Sleep_Amplitude", "SLEEP AMPLITUDE")

# ------ PRINT CONCLUSION ------

cat("\n===== FINAL CONCLUSION =====\n")
cat("The best wake predictor for Sleep Slope based on:\n")
cat("- Cross-validated Conditional R²:", slope_summary$Model[1], "\n")
cat("- AIC:", slope_models$comparison_table$Model[1], "\n")
cat("- BIC:", slope_models$comparison_table %>% arrange(BIC) %>% pull(Model) %>% first(), "\n\n")

cat("The best wake predictor for Sleep Amplitude based on:\n")
cat("- Cross-validated Conditional R²:", amp_summary$Model[1], "\n")
cat("- AIC:", amp_models$comparison_table$Model[1], "\n")
cat("- BIC:", amp_models$comparison_table %>% arrange(BIC) %>% pull(Model) %>% first(), "\n")

# ------ SAVE RESULTS ------

# Save all results
results <- list(
  cv_slope_results = cv_slope_results,
  cv_amp_results = cv_amp_results,
  slope_summary = slope_summary,
  amp_summary = amp_summary,
  slope_models = slope_models,
  amp_models = amp_models,
  plot_slope = plot_slope,
  plot_amp = plot_amp
)

# Save as RData
save(results, file = "wake_sleep_analysis_results.RData")

# Save plots
ggsave("sleep_slope_marginal_r2.png", plot_slope$marginal, width = 8, height = 6)
ggsave("sleep_slope_conditional_r2.png", plot_slope$conditional, width = 8, height = 6)
ggsave("sleep_amplitude_marginal_r2.png", plot_amp$marginal, width = 8, height = 6)
ggsave("sleep_amplitude_conditional_r2.png", plot_amp$conditional, width = 8, height = 6)

cat("\nAnalysis complete. Results saved to wake_sleep_analysis_results.RData\n")