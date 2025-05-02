# Code updated by Claude and Sophia Snipes, children-wake, 2025

library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models
library(effsize)       # For Cohen's d calculation
library(performance)   # For additional R² calculations
library(patchwork)     # For combining plots


################################################################################
### Parameters

dataFilepath <- "WakeSleepAllData.csv"

# sleep parameters to predict
slope_outcome <- "Sleep_Slope_Matched"
amplitude_outcome <- "Sleep_Amplitude"

# components of the mixed effects model
fixed_model <- "~ Age + "
predictors <- c("Amplitude", "Duration", "Offset", "Exponent") # these get looped into the fixed model
random_model <- "+ (1|Participant)"


################################################################################
### functions

### Create participant-based folds with balanced distribution 
# folds are a list of 10 lists, each one missing file indices of 10% of participants

create_balanced_participant_folds <- function(data, k = 10) {
  
  set.seed(120)
  
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


### Calculate marginal and conditional R² for test data
calculate_prediction_metrics <- function(model, test_data, outcome_var) {
  
  # Get predictions from the mixed effects model on test data
  test_predictions <- predict(model, newdata = test_data, allow.new.levels = TRUE)
  
  # Get fixed effects part of the model formula
  fixed_formula <- reformulate(
    termlabels = attr(terms(model), "term.labels")[!grepl("\\|", attr(terms(model), "term.labels"))],
    response = outcome_var
  )
  
  # Fit fixed effects model on test data for marginal R²
  fixed_model_test <- lm(fixed_formula, data = test_data)
  fixed_predictions <- predict(fixed_model_test)
  
  # Calculate total sum of squares
  total_SS <- sum((test_data[[outcome_var]] - mean(test_data[[outcome_var]], na.rm = TRUE))^2, na.rm = TRUE)
  
  # Calculate residual sum of squares after mixed model predictions
  residual_SS <- sum((test_data[[outcome_var]] - test_predictions)^2, na.rm = TRUE)
  
  # Calculate residual sum of squares after fixed effects only
  fixed_residual_SS <- sum((test_data[[outcome_var]] - fixed_predictions)^2, na.rm = TRUE)
  
  # Calculate pseudo-R² values
  conditional_r2 <- 1 - (residual_SS / total_SS)
  marginal_r2 <- 1 - (fixed_residual_SS / total_SS)
  
  # Calculate RMSE on test set
  rmse <- sqrt(mean((test_data[[outcome_var]] - test_predictions)^2, na.rm = TRUE))
  
  # Return results as a list
  return(list(
    marginal_r2 = marginal_r2,
    conditional_r2 = conditional_r2,
    rmse = rmse
  ))
}

### Cross-validation function with R² calculated on test data

cross_validate_mixed_models <- function(data, fixed_model, random_model, outcome_var, predictors, n_folds=10) {
  
  # parameters
  folds <- create_balanced_participant_folds(data, k = n_folds)
  
  # run
  results <- data.frame(  # blank output table
    Fold = integer(),
    Model = character(),
    Marginal_R2 = numeric(),
    Conditional_R2 = numeric(),
    RMSE = numeric()
  )
  
  for (i in 1:n_folds) {
    train <- data[folds[[i]], ]
    test <- data[-folds[[i]], ]
    
    for (pred in predictors) {
      
      # Train model on training data
      formula <- as.formula(paste(outcome_var, fixed_model, pred, random_model))
      model <- lmer(formula, data = train)
      
      
      # Calculate R² metrics using our new function
      r2_metrics <- calculate_prediction_metrics(model, test, outcome_var)
      
      # Add to results table
      results <- results %>% add_row(
        Fold = i,
        Model = pred,
        Marginal_R2 = r2_metrics$marginal_r2,
        Conditional_R2 = r2_metrics$conditional_r2,
        RMSE = r2_metrics$rmse
      )
    }
  }
  
  # Summarize results by model
  summary_results <- results %>%
    group_by(Model) %>%
    summarize(
      Mean_Marginal_R2 = mean(Marginal_R2),
      SD_Marginal_R2 = sd(Marginal_R2),
      Mean_Conditional_R2 = mean(Conditional_R2),
      SD_Conditional_R2 = sd(Conditional_R2),
      Mean_RMSE = mean(RMSE),
      SD_RMSE = sd(RMSE)
    )
  
  return(list(
    fold_results = results,
    summary = summary_results
  ))
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
  
  best_r2 <- cv_results$summary %>% 
    filter(Model == best_model) %>% 
    pull(Mean_Marginal_R2)
    
  best_rmse <- cv_results$summary %>% 
    filter(Model == best_model) %>% 
    pull(Mean_RMSE)
  
  for(pred in predictors) {
    model_aic <- aic_values %>% filter(Model == pred) %>% pull(AIC)
    model_bic <- aic_values %>% filter(Model == pred) %>% pull(BIC)
    
    if(pred != best_model) {
      model_r2 <- cv_results$summary %>% 
        filter(Model == pred) %>% 
        pull(Mean_Marginal_R2)
        
      model_rmse <- cv_results$summary %>% 
        filter(Model == pred) %>% 
        pull(Mean_RMSE)
      
      # Calculate Cohen's d for R2 (higher is better)
      r2_values_best <- cv_results$fold_results %>% 
        filter(Model == best_model) %>% 
        pull(Marginal_R2)
        
      r2_values_model <- cv_results$fold_results %>% 
        filter(Model == pred) %>% 
        pull(Marginal_R2)
        
      r2_d <- cohen.d(r2_values_best, r2_values_model)$estimate
      
      # Calculate Cohen's d for RMSE (lower is better, so flip sign)
      rmse_values_best <- cv_results$fold_results %>% 
        filter(Model == best_model) %>% 
        pull(RMSE)
        
      rmse_values_model <- cv_results$fold_results %>% 
        filter(Model == pred) %>% 
        pull(RMSE)
        
      rmse_d <- cohen.d(rmse_values_model, rmse_values_best)$estimate * -1
      
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
  best_model <- cv_results$summary %>%
    arrange(desc(Mean_Marginal_R2)) %>%
    pull(Model) %>%
    first()
  
  # Get all model names
  models <- unique(cv_results$fold_results$Model)
  
  # Create results dataframe
  comparison_results <- data.frame(
    Best_Model = character(),
    Compared_Model = character(),
    Wilcox_p_value = numeric()
  )
  
  # Compare with each other model
  for (model in models) {
    if (model != best_model) {
      # Get R² values for both models
      best_r2 <- cv_results$fold_results %>% 
        filter(Model == best_model) %>% 
        pull(Marginal_R2)
      
      model_r2 <- cv_results$fold_results %>% 
        filter(Model == model) %>% 
        pull(Marginal_R2)
      
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
    r2_stats <- cv_results$summary %>%
      filter(Model == pred)
    
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
      Marginal_R2_Mean = r2_stats$Mean_Marginal_R2,
      Marginal_R2_SD = r2_stats$SD_Marginal_R2,
      Conditional_R2_Mean = r2_stats$Mean_Conditional_R2,
      Conditional_R2_SD = r2_stats$SD_Conditional_R2,
      RMSE_Mean = r2_stats$Mean_RMSE,
      RMSE_SD = r2_stats$SD_RMSE,
      Fixed_Effect_Estimate = estimate,
      Fixed_Effect_p = p_value,
      Effect_Size_vs_Best_AIC = effect_size,
      Wilcox_p_value = wilcox_p
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


################################################################################
### Plot function to visualize R² values

plot_r2_boxplots <- function(cv_results, outcome_name) {
  # Prepare data for plotting
  plot_data <- cv_results$fold_results %>%
    pivot_longer(
      cols = c(Marginal_R2, Conditional_R2),
      names_to = "R2_Type",
      values_to = "R2_Value"
    )
  
  # Create plot with two subplots
  # 1. Marginal R² boxplot
  p_marginal <- ggplot(
    plot_data %>% filter(R2_Type == "Marginal_R2"),
    aes(x = Model, y = R2_Value, fill = Model)
  ) +
  geom_boxplot() +
  labs(
    title = paste("Marginal R² for", outcome_name),
    x = "Predictor",
    y = "Marginal R²"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 9)
  )
  
  # 2. Conditional R² boxplot
  p_conditional <- ggplot(
    plot_data %>% filter(R2_Type == "Conditional_R2"),
    aes(x = Model, y = R2_Value, fill = Model)
  ) +
  geom_boxplot() +
  labs(
    title = paste("Conditional R² for", outcome_name),
    x = "Predictor",
    y = "Conditional R²"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 9)
  )
  
  # Combine plots horizontally
  combined_plot <- p_marginal + p_conditional
  
  return(combined_plot)
}

################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# remove NA values
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# Perform cross-validation with R² calculated on test sets
cv_slope_results <- cross_validate_mixed_models(data_slope, fixed_model, random_model, slope_outcome, predictors)
cv_amp_results <- cross_validate_mixed_models(data_amp, fixed_model, random_model, amplitude_outcome, predictors)

# Create and display R² boxplots
slope_r2_plot <- plot_r2_boxplots(cv_slope_results, "Sleep Slope")
amplitude_r2_plot <- plot_r2_boxplots(cv_amp_results, "Sleep Amplitude")

# Display plots
print(slope_r2_plot)
print(amplitude_r2_plot)

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