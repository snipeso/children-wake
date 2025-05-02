library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models
library(effsize)       # For Cohen's d calculation


################################################################################
### Parameters

dataFilepath <- "WakeSleepAllData.csv"

slope_outcome <- "Sleep_Slope_Matched"
amplitude_outcome <- "Sleep_Amplitude"

fixed_model <- "~ Age + "
predictors <- c("Amplitude", "Duration", "Offset", "Exponent")  # Variables to model
random_model <- "+ (1|Participant)"


################################################################################
### functions

### Create participant-based folds with balanced distribution 
# folds are a list of 10 lists, each one missing file indices of 10% of participants


create_balanced_participant_folds <- function(data, k = 10) {
  
  set.seed(100)
  
  # randomize participants
  participants <- unique(data$Participant)
  shuffled_participants <- sample(participants)
  
  # Calculate how many participants per fold (roughly equal)
  n_participants <- length(participants)
  n_per_fold <- ceiling(n_participants / k)
  
  # Assign participants to folds
  participant_fold_map <- data.frame(
    Participant = shuffled_participants,
    Fold = rep(1:k, each = n_per_fold)[1:n_participants]
  )
  
  # Create a list of training indices for each fold
  folds <- list()
  for (i in 1:k) {
    
    # Get participants in this fold
    fold_participants <- participant_fold_map$Participant[participant_fold_map$Fold == i]
    
    # Get indices for all data points NOT from these participants (for training)
    train_indices <- which(!(data$Participant %in% fold_participants))
    folds[[i]] <- train_indices
  }
  
  return(folds)
}


### Cross-validation function

cross_validate_mixed_models <- function(data, fixed_model, random_model, outcome_var, predictors, k=10) {
  
  # parameters
  folds <- create_balanced_participant_folds(data, k)
  
  # run
  results <- data.frame(  # blank output table
    Fold = integer(),
    Model = character(),
    Marginal_R2 = numeric(),
    Conditional_R2 = numeric(),
    RMSE = numeric()
  )
  for (i in 1:k) {
    train <- data[folds[[i]], ]
    test <- data[-folds[[i]], ]
    
    # Loop through predictors
    for (pred in predictors) {
      formula <- as.formula(paste(outcome_var, fixed_model, pred, random_model))
      
      tryCatch({
        model <- lmer(formula, data = train)
        r2_values <- r.squaredGLMM(model)
        
        # Calculate RMSE on test set
        predictions <- predict(model, newdata = test, allow.new.levels = TRUE)
        rmse <- sqrt(mean((test[[outcome_var]] - predictions)^2, na.rm = TRUE))
        
        # Add to results
        results <- results %>% add_row(
          Fold = i,
          Model = pred,
          Marginal_R2 = r2_values[1],
          Conditional_R2 = r2_values[2],
          RMSE = rmse
        )
      }, error = function(e) {
        warning(paste("Error in fold", i, "with predictor", pred, ":", e$message))
      })
    }
  }
  
  return(results)
}



### Calculate effect size compared to best AIC model

calculate_effect_sizes <- function(data, outcome_var, cv_results) {
  # Find best AIC model
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  # Calculate AIC for each model
  aic_values <- data.frame(Model = character(), AIC = numeric(), BIC = numeric())
  for(pred in predictors) {
    formula <- as.formula(paste(outcome_var, "~ Age + ", pred, "+ (1|Participant)"))
    model <- lmer(formula, data = data)
    aic_values <- aic_values %>% add_row(
      Model = pred, 
      AIC = AIC(model),
      BIC = BIC(model)
    )
  }
  
  # Find best AIC model
  best_model <- aic_values %>% 
    arrange(AIC) %>% 
    slice(1) %>% 
    pull(Model)
  
  # Calculate effect sizes against best model
  effect_sizes <- data.frame(
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    R2_Effect_Size = numeric(),
    RMSE_Effect_Size = numeric()
  )
  
  best_r2 <- cv_results %>% filter(Model == best_model) %>% pull(Marginal_R2)
  best_rmse <- cv_results %>% filter(Model == best_model) %>% pull(RMSE)
  
  for(pred in predictors) {
    model_aic <- aic_values %>% filter(Model == pred) %>% pull(AIC)
    model_bic <- aic_values %>% filter(Model == pred) %>% pull(BIC)
    
    if(pred != best_model) {
      model_r2 <- cv_results %>% filter(Model == pred) %>% pull(Marginal_R2)
      model_rmse <- cv_results %>% filter(Model == pred) %>% pull(RMSE)
      
      # Calculate Cohen's d for R2 (higher is better)
      r2_d <- cohen.d(best_r2, model_r2)$estimate
      
      # Calculate Cohen's d for RMSE (lower is better, so flip sign)
      rmse_d <- cohen.d(model_rmse, best_rmse)$estimate * -1
      
      effect_sizes <- effect_sizes %>% add_row(
        Model = pred,
        AIC = model_aic,
        BIC = model_bic,
        R2_Effect_Size = r2_d,
        RMSE_Effect_Size = rmse_d
      )
    } else {
      effect_sizes <- effect_sizes %>% add_row(
        Model = pred,
        AIC = model_aic,
        BIC = model_bic,
        R2_Effect_Size = NA,
        RMSE_Effect_Size = NA
      )
    }
  }
  
  return(list(
    best_model = best_model,
    effect_sizes = effect_sizes
  ))
}



### Simplified function for Wilcoxon tests comparing best model to others


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



### Modified fit_final_models function to include R² standard deviations


fit_final_models <- function(data, outcome_var, outcome_name, wilcox_comparisons, cv_results, effect_sizes_info) {
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  best_model <- wilcox_comparisons$best_model
  
  comparison_table <- data.frame(
    Outcome = character(),
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    Marginal_R2_Mean = numeric(),
    Marginal_R2_SD = numeric(),
    Conditional_R2_Mean = numeric(),
    Conditional_R2_SD = numeric(),
    RMSE_Mean = numeric(),
    RMSE_SD = numeric(),
    Fixed_Effect_Estimate = numeric(),
    Fixed_Effect_p = numeric(),
    Effect_Size_vs_Best_AIC = numeric(),
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
        Conditional_R2_SD = sd(Conditional_R2),
        RMSE_Mean = mean(RMSE),
        RMSE_SD = sd(RMSE)
      )
    
    formula <- as.formula(paste(outcome_var, "~ Age +", pred, "+ (1|Participant)"))
    model <- lmer(formula, data = data)
    
    model_aic <- AIC(model)
    model_bic <- BIC(model)
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
    
    # Get effect size vs best AIC model
    effect_size <- NA
    if (pred != effect_sizes_info$best_model) {
      es_row <- effect_sizes_info$effect_sizes %>% 
        filter(Model == pred)
      if (nrow(es_row) > 0) {
        effect_size <- es_row$R2_Effect_Size
      }
    }
    
    comparison_table <- comparison_table %>% add_row(
      Outcome = outcome_name,
      Model = pred,
      AIC = model_aic,
      BIC = model_bic,
      Marginal_R2_Mean = r2_stats$Marginal_R2_Mean,
      Marginal_R2_SD = r2_stats$Marginal_R2_SD,
      Conditional_R2_Mean = r2_stats$Conditional_R2_Mean,
      Conditional_R2_SD = r2_stats$Conditional_R2_SD,
      RMSE_Mean = r2_stats$RMSE_Mean,
      RMSE_SD = r2_stats$RMSE_SD,
      Fixed_Effect_Estimate = estimate,
      Fixed_Effect_p = p_value,
      Effect_Size_vs_Best_AIC = effect_size,
      Wilcox_p_value = wilcox_p
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# remove NA values
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# Perform cross-validation
cv_slope_results <- cross_validate_mixed_models(data_slope, fixed_model, random_model, slope_outcome, predictors)
cv_amp_results <- cross_validate_mixed_models(data_amp, fixed_model, random_model, amplitude_outcome, predictors)

# Run Wilcoxon comparisons
slope_comparisons <- compare_to_best_model(cv_slope_results)
amp_comparisons <- compare_to_best_model(cv_amp_results)

# Calculate effect sizes compared to best AIC model
slope_effect_sizes <- calculate_effect_sizes(data_slope, "Sleep_Slope_Matched", cv_slope_results)
amp_effect_sizes <- calculate_effect_sizes(data_amp, "Sleep_Amplitude", cv_amp_results)

# Final model tables with effect sizes and BIC
slope_models <- fit_final_models(data_slope, "Sleep_Slope_Matched", "Sleep Slope", slope_comparisons, cv_slope_results, slope_effect_sizes)
amp_models <- fit_final_models(data_amp, "Sleep_Amplitude", "Sleep Amplitude", amp_comparisons, cv_amp_results, amp_effect_sizes)

# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)
