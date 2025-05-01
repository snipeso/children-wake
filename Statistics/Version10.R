library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models


################################################################################
### functions

# Cross-validation function
cross_validate_mixed_models <- function(data, outcome_var, k=10) {
  # Create a results dataframe
  results <- data.frame(
    Fold = integer(),
    Model = character(),
    Marginal_R2 = numeric(),
    Conditional_R2 = numeric()
  )
  
  # Create participant-based folds
  set.seed(100)
  participants <- unique(data$Participant)
  participant_folds <- sample(1:k, length(participants), replace=TRUE)
  names(participant_folds) <- participants
  
  folds <- list()
  for(i in 1:k) {
    fold_participants <- names(participant_folds[participant_folds == i])
    train_indices <- which(!(data$Participant %in% fold_participants))
    folds[[i]] <- train_indices
  }
  
  # Variables to model
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  # Loop through folds
  for (i in 1:k) {
    train <- data[folds[[i]], ]
    test <- data[-folds[[i]], ]
    
    # Skip if either set is empty
    if(nrow(train) == 0 || nrow(test) == 0) {
      next
    }
    
    # Check for sufficient participants
    train_participants <- length(unique(train$Participant))
    test_participants <- length(unique(test$Participant))
    if(train_participants < 2 || test_participants < 2) {
      next
    }
    
    # Loop through predictors
    for (pred in predictors) {
      formula <- as.formula(paste(outcome_var, "~ Age + ", pred, "+ (1|Participant)"))
      
      tryCatch({
        model <- lmer(formula, data = train)
        r2_values <- r.squaredGLMM(model)
        
        # Add to results
        results <- results %>% add_row(
          Fold = i,
          Model = pred,
          Marginal_R2 = r2_values[1],
          Conditional_R2 = r2_values[2]
        )
      }, error = function(e) {
        warning(paste("Error in fold", i, "with predictor", pred, ":", e$message))
      })
    }
  }
  
  return(results)
}



# Simplified function for Wilcoxon tests comparing best model to others
compare_to_best_model <- function(cv_results) {
  # Find best model based on mean R²
  best_model <- cv_results %>%
    group_by(Model) %>%
    summarize(Mean_R2 = mean(Marginal_R2)) %>%
    arrange(desc(Mean_R2)) %>%
    pull(Model) %>%
    first()
  
  # Get R² values for best model
  best_r2 <- cv_results %>% filter(Model == best_model) %>% pull(Marginal_R2)
  
  # Get all model names
  models <- unique(cv_results$Model)
  
  # Create results dataframe
  comparison_results <- data.frame(
    Best_Model = character(),
    Compared_Model = character(),
    Wilcox_p_value = numeric()
  )
  
  # Compare with each other model
  for (model in models) {
    if (model != best_model) {
      # Get R² values for this model
      model_r2 <- cv_results %>% filter(Model == model) %>% pull(Marginal_R2)
      
      # Perform Wilcoxon test (is best model better?)
      test_result <- wilcox.test(best_r2, model_r2, paired = TRUE, alternative = "greater")
      
      # Add to results
      comparison_results <- comparison_results %>% add_row(
        Best_Model = best_model,
        Compared_Model = model,
        Wilcox_p_value = test_result$p.value
      )
    }
  }
  
  return(list(
    best_model = best_model,
    comparisons = comparison_results
  ))
}

# Modified fit_final_models function to include R² standard deviations
fit_final_models <- function(data, outcome_var, outcome_name, wilcox_comparisons, cv_results) {
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  best_model <- wilcox_comparisons$best_model
  
  comparison_table <- data.frame(
    Outcome = character(),
    Model = character(),
    AIC = numeric(),
    Marginal_R2_Mean = numeric(),
    Marginal_R2_SD = numeric(),
    Conditional_R2_Mean = numeric(),
    Conditional_R2_SD = numeric(),
    Fixed_Effect_Estimate = numeric(),
    Fixed_Effect_p = numeric(),
    Wilcox_p_value = numeric()
  )
  
  for (pred in predictors) {
    # Get R² statistics from cross-validation
    r2_stats <- cv_results %>%
      filter(Model == pred) %>%
      summarize(
        Marginal_R2_Mean = mean(Marginal_R2),
        Marginal_R2_SD = sd(Marginal_R2),
        Conditional_R2_Mean = mean(Conditional_R2),
        Conditional_R2_SD = sd(Conditional_R2)
      )
    
    formula <- as.formula(paste(outcome_var, "~ Age +", pred, "+ (1|Participant)"))
    model <- lmer(formula, data = data)
    
    model_aic <- AIC(model)
    coefs <- summary(model)$coefficients
    estimate <- coefs[pred, "Estimate"]
    p_value <- coefs[pred, "Pr(>|t|)"]
    
    # Get Wilcoxon p-value
    wilcox_p <- NA
    if (pred != best_model) {
      wilcox_row <- wilcox_comparisons$comparisons %>% 
        filter(Compared_Model == pred)
      if (nrow(wilcox_row) > 0) {
        wilcox_p <- wilcox_row$Wilcox_p_value
      }
    }
    
    comparison_table <- comparison_table %>% add_row(
      Outcome = outcome_name,
      Model = pred,
      AIC = model_aic,
      Marginal_R2_Mean = r2_stats$Marginal_R2_Mean,
      Marginal_R2_SD = r2_stats$Marginal_R2_SD,
      Conditional_R2_Mean = r2_stats$Conditional_R2_Mean,
      Conditional_R2_SD = r2_stats$Conditional_R2_SD,
      Fixed_Effect_Estimate = estimate,
      Fixed_Effect_p = p_value,
      Wilcox_p_value = wilcox_p
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


################################################################################
### run

# Read data
data <- read.csv("WakeSleepAllData.csv")

# remove NA values
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# Perform cross-validation
cv_slope_results <- cross_validate_mixed_models(data_slope, "Sleep_Slope_Matched")
cv_amp_results <- cross_validate_mixed_models(data_amp, "Sleep_Amplitude")


# Run Wilcoxon comparisons
slope_comparisons <- compare_to_best_model(cv_slope_results)
amp_comparisons <- compare_to_best_model(cv_amp_results)


# Final model tables with R² standard deviations
slope_models <- fit_final_models(data_slope, "Sleep_Slope_Matched", "Sleep Slope", slope_comparisons, cv_slope_results)
amp_models <- fit_final_models(data_amp, "Sleep_Amplitude", "Sleep Amplitude", amp_comparisons, cv_amp_results)

# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)
