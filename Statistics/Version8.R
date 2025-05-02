# Library loading - consistent use of tidyverse and related packages
library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models
library(caret)         # For cross-validation
library(knitr)         # For table creation
library(kableExtra)    # For enhanced tables
library(lmtest)        # For likelihood ratio tests
library(boot)          # For bootstrapping

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Subset data to remove NA values - using tidyverse approach
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# ------ CROSS-VALIDATION APPROACH WITH ESTABLISHED FUNCTIONS ------

# Function to perform k-fold cross-validation for mixed-effects models with proper handling of clustered data
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
  
  # Create grouped folds - keeping all observations from the same participant together
  set.seed(123)
  if("Participant" %in% colnames(data)) {
    # Get unique participants
    participants <- unique(data$Participant)
    
    # Randomly assign participants to folds
    participant_folds <- sample(1:k, length(participants), replace=TRUE)
    names(participant_folds) <- participants
    
    # Create empty list to store fold indices
    folds <- list()
    for(i in 1:k) {
      # Get participants in this fold
      fold_participants <- names(participant_folds[participant_folds == i])
      
      # Get indices of all observations from other participants (for training)
      train_indices <- which(!(data$Participant %in% fold_participants))
      
      # Store training indices
      folds[[i]] <- train_indices
    }
  } else {
    # If no participant column, use standard createFolds
    folds <- createFolds(1:nrow(data), k = k, returnTrain = TRUE)
  }
  
  # Variables to model
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  # Loop through folds
  for (i in 1:k) {
    # Get training and test data
    train <- data[folds[[i]], ]
    test <- data[-folds[[i]], ]
    
    # Skip if either set is empty (can happen with unbalanced groups)
    if(nrow(train) == 0 || nrow(test) == 0) {
      next
    }
    
    # Check if we have enough participants in each set
    if("Participant" %in% colnames(data)) {
      train_participants <- length(unique(train$Participant))
      test_participants <- length(unique(test$Participant))
      
      # Skip if we don't have at least 2 participants in each set
      if(train_participants < 2 || test_participants < 2) {
        warning(paste("Skipping fold", i, "due to insufficient participants in train or test set"))
        next
      }
    }
    
    # Loop through predictors to create and evaluate models
    for (pred in predictors) {
      # Create formula
     #  formula <- as.formula(paste(outcome_var, "~ Age + ", pred, "+ (1|Participant)"))
      formula <- as.formula(paste(outcome_var, "~ Age + Hour + ", pred, "+ (1|Participant)"))
      
      # Fit model on training data
      tryCatch({
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
      }, error = function(e) {
        warning(paste("Error in fold", i, "with predictor", pred, ":", e$message))
      })
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

# ------ COMPARE MARGINAL R² VALUES BETWEEN MODELS USING WILCOXON TESTS ------

# Function to compare marginal R² distributions between models
compare_r2_distributions <- function(cv_results, outcome_name) {
  cat("\n===== WILCOXON TESTS FOR MARGINAL R² COMPARISONS -", outcome_name, "=====\n")
  
  # Get all models
  models <- unique(cv_results$Model)
  
  # Create results table
  comparison_results <- data.frame(
    Model1 = character(),
    Model2 = character(),
    Wilcox_p_value = numeric(),
    Model1_Mean_R2 = numeric(),
    Model2_Mean_R2 = numeric(),
    Is_Model1_Better = logical()
  )
  
  # Compare each pair of models
  for (i in 1:(length(models)-1)) {
    for (j in (i+1):length(models)) {
      model1 <- models[i]
      model2 <- models[j]
      
      # Get R² values for both models
      model1_r2 <- cv_results %>% filter(Model == model1) %>% pull(Marginal_R2)
      model2_r2 <- cv_results %>% filter(Model == model2) %>% pull(Marginal_R2)
      
      # Calculate mean R² for each model
      model1_mean_r2 <- mean(model1_r2)
      model2_mean_r2 <- mean(model2_r2)
      
      # Determine which model has higher mean R²
      if (model1_mean_r2 > model2_mean_r2) {
        # Test if model1 > model2
        test_result <- wilcox.test(model1_r2, model2_r2, paired = TRUE, alternative = "greater")
        is_model1_better <- test_result$p.value < 0.05
      } else {
        # Test if model2 > model1
        test_result <- wilcox.test(model2_r2, model1_r2, paired = TRUE, alternative = "greater")
        is_model1_better <- FALSE
      }
      
      # Add to results
      comparison_results <- comparison_results %>% add_row(
        Model1 = model1,
        Model2 = model2,
        Wilcox_p_value = test_result$p.value,
        Model1_Mean_R2 = model1_mean_r2,
        Model2_Mean_R2 = model2_mean_r2,
        Is_Model1_Better = is_model1_better
      )
    }
  }
  
  # Sort by p-value
  comparison_results <- comparison_results %>% arrange(Wilcox_p_value)
  
  # Print the table
  print(kable(comparison_results, digits = 3, 
              caption = paste("Wilcoxon Tests for", outcome_name, "Marginal R² Comparisons")) %>%
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  # Also compare the best model against all others
  best_model <- cv_results %>% 
    group_by(Model) %>% 
    summarize(Mean_R2 = mean(Marginal_R2)) %>% 
    arrange(desc(Mean_R2)) %>% 
    pull(Model) %>% 
    first()
  
  cat("\nComparing best model (", best_model, ") against all others:\n")
  
  best_model_comparisons <- data.frame(
    Best_Model = character(),
    Compared_Model = character(),
    Wilcox_p_value = numeric(),
    Is_Best_Better = logical()
  )
  
  # Get R² values for best model
  best_r2 <- cv_results %>% filter(Model == best_model) %>% pull(Marginal_R2)
  
  # Compare with each other model
  for (model in models) {
    if (model != best_model) {
      # Get R² values for this model
      model_r2 <- cv_results %>% filter(Model == model) %>% pull(Marginal_R2)
      
      # Perform Wilcoxon test (is best model better?)
      test_result <- wilcox.test(best_r2, model_r2, paired = TRUE, alternative = "greater")
      
      # Add to results
      best_model_comparisons <- best_model_comparisons %>% add_row(
        Best_Model = best_model,
        Compared_Model = model,
        Wilcox_p_value = test_result$p.value,
        Is_Best_Better = test_result$p.value < 0.05
      )
    }
  }
  
  # Print the best model comparisons
  print(kable(best_model_comparisons, digits = 3) %>%
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  return(list(
    all_comparisons = comparison_results,
    best_comparisons = best_model_comparisons
  ))
}

# Run the comparisons for both outcomes
slope_r2_comparisons <- compare_r2_distributions(cv_slope_results, "Sleep Slope")
amp_r2_comparisons <- compare_r2_distributions(cv_amp_results, "Sleep Amplitude")

# ------ FIT FINAL MODELS AND COMPARE ------

# Note: We're replacing the bootstrap approach with a more direct likelihood ratio test
# that compares each model to a null model without the wake predictor
# This provides a p-value that directly tests if adding the wake predictor 
# significantly improves the model, which is equivalent to testing if R² > 0

# Function to fit and compare models with model fit statistics
fit_and_compare_models <- function(data, outcome_var, outcome_name) {
  cat("\n===== FINAL MODEL COMPARISON FOR", outcome_name, "=====\n")
  
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
    Outcome = character(),
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    Marginal_R2 = numeric(),
    Conditional_R2 = numeric(),
    Fixed_Effect_Estimate = numeric(),
    Fixed_Effect_SE = numeric(),
    Fixed_Effect_p = numeric(),
    R2_greater_than_zero_p = numeric()
  )
  
  # Create null model (without any wake predictor) for testing if R² > 0
  null_formula <- as.formula(paste(outcome_var, "~ Age + (1|Participant)"))
  null_model <- lmer(null_formula, data = data)
  
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
    
    # Test if wake predictor significantly improves model compared to null model (test if R² > 0)
    r2_p_value <- NA
    tryCatch({
      # Compare the model with predictor against null model (without predictor)
      r2_lrt <- anova(null_model, model)
      r2_p_value <- r2_lrt["Pr(>Chisq)"][nrow(r2_lrt), ]
    }, error = function(e) {
      cat("Error in R² significance test for", pred, ":", e$message, "\n")
    })
    
    # Add to table
    comparison_table <- comparison_table %>% add_row(
      Outcome = outcome_name,
      Model = pred,
      AIC = model_aic,
      BIC = model_bic,
      Marginal_R2 = r2_values[1],
      Conditional_R2 = r2_values[2],
      Fixed_Effect_Estimate = estimate,
      Fixed_Effect_SE = se,
      Fixed_Effect_p = p_value,
      R2_greater_than_zero_p = r2_p_value
    )
  }
  
  # Sort by AIC within each outcome
  comparison_table <- comparison_table %>% arrange(AIC)
  
  # Print the table
  cat("\nModel comparison table for", outcome_name, ":\n")
  print(kable(comparison_table, digits = 3) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed")))
  
  return(list(
    comparison_table = comparison_table,
    models = models,
    null_model = null_model
  ))
}

# Fit and compare models for both outcomes
slope_models <- fit_and_compare_models(data_slope, "Sleep_Slope_Matched", "Sleep Slope")
amp_models <- fit_and_compare_models(data_amp, "Sleep_Amplitude", "Sleep Amplitude")

# ------ COMBINED RESULTS TABLE ------

# Combine comparison tables
combined_table <- rbind(
  slope_models$comparison_table,
  amp_models$comparison_table
)

# Format the table with kable for better presentation
kable(combined_table, digits = 3, 
      caption = "Combined Model Comparison for Sleep Slope and Sleep Amplitude",
      col.names = c("Outcome", "Model", "AIC", "BIC", "Marginal R²", "Conditional R²", 
                    "Fixed Effect Est.", "Fixed Effect SE", "Fixed Effect p", 
                    "R² > 0 p-value")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(which(combined_table$Model == slope_summary$Model[1] & combined_table$Outcome == "Sleep Slope"), background = "#e6f7ff") %>%  # Highlight best models
  row_spec(which(combined_table$Model == amp_summary$Model[1] & combined_table$Outcome == "Sleep Amplitude"), background = "#e6f7ff") %>%  # Highlight best models
  collapse_rows(columns = 1, valign = "middle") %>%  # Merge cells in Outcome column
  print()

# ------ PRINT CONCLUSION ------

cat("\n===== FINAL CONCLUSION =====\n")
cat("The best wake predictor for Sleep Slope based on:\n")
cat("- Cross-validated Conditional R²:", slope_summary$Model[1], "\n")
cat("- AIC:", slope_models$comparison_table$Model[1], "\n")
cat("- BIC:", slope_models$comparison_table %>% arrange(BIC) %>% pull(Model) %>% first(), "\n\n")

cat("Statistical significance of Sleep Slope R² comparisons:\n")
for (i in 1:nrow(slope_r2_comparisons$best_comparisons)) {
  row <- slope_r2_comparisons$best_comparisons[i, ]
  if (row$Is_Best_Better) {
    cat("- ", row$Best_Model, "has significantly higher marginal R² than", row$Compared_Model, 
        "(p =", format(row$Wilcox_p_value, digits = 3), ")\n")
  } else {
    cat("- ", row$Best_Model, "does NOT have significantly higher marginal R² than", row$Compared_Model, 
        "(p =", format(row$Wilcox_p_value, digits = 3), ")\n")
  }
}

cat("\nThe best wake predictor for Sleep Amplitude based on:\n")
cat("- Cross-validated Conditional R²:", amp_summary$Model[1], "\n")
cat("- AIC:", amp_models$comparison_table$Model[1], "\n")
cat("- BIC:", amp_models$comparison_table %>% arrange(BIC) %>% pull(Model) %>% first(), "\n\n")

cat("Statistical significance of Sleep Amplitude R² comparisons:\n")
for (i in 1:nrow(amp_r2_comparisons$best_comparisons)) {
  row <- amp_r2_comparisons$best_comparisons[i, ]
  if (row$Is_Best_Better) {
    cat("- ", row$Best_Model, "has significantly higher marginal R² than", row$Compared_Model, 
        "(p =", format(row$Wilcox_p_value, digits = 3), ")\n")
  } else {
    cat("- ", row$Best_Model, "does NOT have significantly higher marginal R² than", row$Compared_Model, 
        "(p =", format(row$Wilcox_p_value, digits = 3), ")\n")
  }
}

# ------ SAVE RESULTS ------

# Save all results
results <- list(
  cv_slope_results = cv_slope_results,
  cv_amp_results = cv_amp_results,
  slope_summary = slope_summary,
  amp_summary = amp_summary,
  slope_models = slope_models,
  amp_models = amp_models,
  combined_table = combined_table,
  plot_slope = plot_slope,
  plot_amp = plot_amp,
  slope_r2_comparisons = slope_r2_comparisons,
  amp_r2_comparisons = amp_r2_comparisons
)

# Save as RData
save(results, file = "wake_sleep_analysis_results.RData")

# Save plots
ggsave("sleep_slope_marginal_r2.png", plot_slope$marginal, width = 8, height = 6)
ggsave("sleep_slope_conditional_r2.png", plot_slope$conditional, width = 8, height = 6)
ggsave("sleep_amplitude_marginal_r2.png", plot_amp$marginal, width = 8, height = 6)
ggsave("sleep_amplitude_conditional_r2.png", plot_amp$conditional, width = 8, height = 6)

# Save combined table
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)

# Save Wilcoxon test results
write.csv(bind_rows(
  slope_r2_comparisons$all_comparisons %>% mutate(Outcome = "Sleep Slope"),
  amp_r2_comparisons$all_comparisons %>% mutate(Outcome = "Sleep Amplitude")
), "r2_wilcoxon_comparisons.csv", row.names = FALSE)

cat("\nAnalysis complete. Results saved to wake_sleep_analysis_results.RData\n")
cat("Combined table saved to combined_model_comparison.csv\n")
cat("Wilcoxon test results saved to r2_wilcoxon_comparisons.csv\n")