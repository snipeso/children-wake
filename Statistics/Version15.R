# Code adapted by Claude, May 2025
# Added repeated cross-validation to Version14.R

library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models
library(effsize)       # For Cohen's d calculation
library(performance)   # For additional R² calculations
library(patchwork)     # For combining plots
library(bootES)

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

# for cross-validation
n_folds <- 10       
n_repetitions <- 10

# for bootstrap
n_bootstrap <- 1000 # repetitions
conf_level <- 0.95  # 95% confidence interval

################################################################################
### functions

### Create participant-based folds with balanced distribution 
# folds are a list of 10 lists, each one missing file indices of 10% of participants

create_balanced_participant_folds <- function(data, k = 10, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
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


### Calculate predictive R² for test data

calculate_prediction_metrics <- function(model, test_data, outcome_var) {
  # Get actual values from test data
  actual_values <- test_data[[outcome_var]]
  
  # Predict values for test data
  predicted_values <- predict(model, newdata = test_data, allow.new.levels = TRUE)
  
  # Calculate residuals
  residuals <- actual_values - predicted_values
  
  # Calculate total sum of squares (TSS)
  mean_actual <- mean(actual_values)
  tss <- sum((actual_values - mean_actual)^2)
  
  # Calculate residual sum of squares (RSS)
  rss <- sum(residuals^2)
  
  # Calculate predictive R²
  predictive_r2 <- 1 - (rss / tss)
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2))
  
  # Return results
  return(list(
    predictive_r2 = predictive_r2,
    rmse = rmse
  ))
}


### Repeated Cross-validation

repeated_cross_validate_mixed_models <- function(data, fixed_model, random_model, outcome_var, 
                                                 predictors, n_folds=10, n_repetitions=10) {
  
  # Initialize results dataframe
  results <- data.frame(
    Repetition = integer(),
    Fold = integer(),
    Model = character(),
    Predictive_R2 = numeric(),
    RMSE = numeric()
  )
  
  # Run repeated cross-validation
  for (rep in 1:n_repetitions) {
    # Create new folds for each repetition with different seed (little hack that makes the seeds just the index of repetitions)
    folds <- create_balanced_participant_folds(data, k = n_folds, seed = rep)
    
    for (i in 1:n_folds) {
      train <- data[folds[[i]], ]
      test <- data[-folds[[i]], ]
      
      for (pred in predictors) {
        
        # Train model on training data
        formula <- as.formula(paste(outcome_var, fixed_model, pred, random_model))
        model <- lmer(formula, data = train)
        
        # Calculate predictive R² metrics on testing data
        r2_metrics <- calculate_prediction_metrics(model, test, outcome_var)
        
        # Add to results table
        results <- results %>% add_row(
          Repetition = rep,
          Fold = i,
          Model = pred,
          Predictive_R2 = r2_metrics$predictive_r2,
          RMSE = r2_metrics$rmse
        )
      }
    }
  }
  
  # Summarize results by model
  summary_results <- results %>%
    group_by(Model) %>%
    summarize(
      Mean_Predictive_R2 = mean(Predictive_R2),
      SD_Predictive_R2 = sd(Predictive_R2),
      Mean_RMSE = mean(RMSE),
      SD_RMSE = sd(RMSE)
    )
  
  return(list(
    fold_results = results,
    summary = summary_results
  ))
}


### Calculate effect size compared to best AIC model

calculate_effect_sizes <- function(data, outcome_var, cv_results, predictors) {
  # Find best AIC model
  
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
    pull(Mean_Predictive_R2)
  
  best_rmse <- cv_results$summary %>% 
    filter(Model == best_model) %>% 
    pull(Mean_RMSE)
  
  for(pred in predictors) {
    model_aic <- aic_values %>% filter(Model == pred) %>% pull(AIC)
    model_bic <- aic_values %>% filter(Model == pred) %>% pull(BIC)
    
    if(pred != best_model) {
      model_r2 <- cv_results$summary %>% 
        filter(Model == pred) %>% 
        pull(Mean_Predictive_R2)
      
      model_rmse <- cv_results$summary %>% 
        filter(Model == pred) %>% 
        pull(Mean_RMSE)
      
      # Calculate Cohen's d for R2 (higher is better)
      r2_values_best <- cv_results$fold_results %>% 
        filter(Model == best_model) %>% 
        pull(Predictive_R2)
      
      r2_values_model <- cv_results$fold_results %>% 
        filter(Model == pred) %>% 
        pull(Predictive_R2)
      
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


### Bootstrap-based function for comparing models using confidence intervals

compare_to_best_model <- function(cv_results, n_bootstrap = 1000, conf_level = 0.95) {
  
  # Find best model based on mean predictive R²
  best_model <- cv_results$summary %>%
    arrange(desc(Mean_Predictive_R2)) %>%
    pull(Model) %>%
    first()
  
  # Extract models from results
  models <- unique(cv_results$fold_results$Model)
  
  # Create results dataframe
  comparison_results <- data.frame(
    Best_Model = character(),
    Compared_Model = character(),
    Mean_R2_Diff = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    Bootstrap_p_value = numeric()
  )
  
  # Set seed for reproducibility
  set.seed(42)
  
  # For each model comparison
  for (model in models) {
    if (model != best_model) {
      # Prepare data at repetition level (average across folds)
      rep_data <- cv_results$fold_results %>%
        group_by(Repetition, Model) %>%
        summarize(Mean_R2 = mean(Predictive_R2), .groups = "drop") %>%
        filter(Model %in% c(best_model, model))
      
      # Calculate observed difference
      obs_diff <- rep_data %>%
        group_by(Model) %>%
        summarize(Mean = mean(Mean_R2)) %>%
        pivot_wider(names_from = Model, values_from = Mean) %>%
        mutate(diff = !!sym(best_model) - !!sym(model)) %>%
        pull(diff)
      
      # Run bootstrap analysis on repetition means
      boot_result <- bootES(
        data = rep_data,
        R = n_bootstrap,
        data.col = "Mean_R2", 
        group.col = "Model",
        block.col = "Repetition",  # Account for repetition structure
        contrast = c(best_model, model),
        effect.type = "cohens.d"
      )
      
      # Get confidence interval for mean difference
      ci <- boot_result$bounds.norm$basic[c(1, 3)] * boot_result$sd.pooled
      
      # Compute p-value (proportion of bootstrap samples with diff <= 0)
      p_value <- mean(boot_result$t * boot_result$sd.pooled <= 0)
      
      # Add to results
      comparison_results <- comparison_results %>% add_row(
        Best_Model = best_model,
        Compared_Model = model,
        Mean_R2_Diff = obs_diff,
        CI_Lower = ci[1],
        CI_Upper = ci[2],
        Bootstrap_p_value = p_value
      )
    }
  }
  
  return(list(
    best_model = best_model,
    comparisons = comparison_results
  ))
}


### Modified fit_final_models function for bootstrap comparisons

fit_final_models <- function(data, outcome_var, outcome_name, bootstrap_comparisons, cv_results, effect_sizes_info, predictors) {
  best_model <- bootstrap_comparisons$best_model
  
  comparison_table <- data.frame(
    Outcome = character(),
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    Predictive_R2_Mean = numeric(),
    Predictive_R2_SD = numeric(),
    RMSE_Mean = numeric(),
    RMSE_SD = numeric(),
    Fixed_Effect_Estimate = numeric(),
    Fixed_Effect_p = numeric(),
    Effect_Size_vs_Best_AIC = numeric(),
    Bootstrap_p_value = numeric(),
    R2_Diff_CI_Lower = numeric(),
    R2_Diff_CI_Upper = numeric()
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
    
    # Get bootstrap p-value and confidence intervals
    bootstrap_p <- NA
    ci_lower <- NA
    ci_upper <- NA
    
    if (pred != best_model) {
      bootstrap_row <- bootstrap_comparisons$comparisons %>% 
        filter(Compared_Model == pred)
      if (nrow(bootstrap_row) > 0) {
        bootstrap_p <- bootstrap_row$Bootstrap_p_value
        ci_lower <- bootstrap_row$CI_Lower
        ci_upper <- bootstrap_row$CI_Upper
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
      Predictive_R2_Mean = r2_stats$Mean_Predictive_R2,
      Predictive_R2_SD = r2_stats$SD_Predictive_R2,
      RMSE_Mean = r2_stats$Mean_RMSE,
      RMSE_SD = r2_stats$SD_RMSE,
      Fixed_Effect_Estimate = estimate,
      Fixed_Effect_p = p_value,
      Effect_Size_vs_Best_AIC = effect_size,
      Bootstrap_p_value = bootstrap_p,
      R2_Diff_CI_Lower = ci_lower,
      R2_Diff_CI_Upper = ci_upper
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


### Plot function to visualize predictive R² values

plot_r2_boxplots <- function(cv_results, outcome_name, predictors) {
  plot_data <- cv_results$fold_results
  plot_data$Model <- factor(plot_data$Model, levels = predictors)
  
  p1 <- ggplot(plot_data, aes(x = Model, y = Predictive_R2, fill = Model)) +
    geom_boxplot() +
    labs(title = paste("Predictive R² for", outcome_name), x = "Predictor", y = "Predictive R²") +
    theme_light() + theme(legend.position = "none")
  
  p2 <- ggplot(plot_data, aes(x = Model, y = RMSE, fill = Model)) +
    geom_boxplot() +
    labs(title = paste("RMSE for", outcome_name), x = "Predictor", y = "RMSE") +
    theme_light() + theme(legend.position = "none")
  
  print(p1 + p2)
}

#  add repetition and fold counts to plot titles
plot_r2_boxplots_with_counts <- function(cv_results, outcome_name, predictors) {
  plot_data <- cv_results$fold_results
  plot_data$Model <- factor(plot_data$Model, levels = predictors)
  
  # Get repetition and fold counts
  n_reps <- length(unique(plot_data$Repetition))
  n_folds <- length(unique(plot_data$Fold))
  
  p1 <- ggplot(plot_data, aes(x = Model, y = Predictive_R2, fill = Model)) +
    geom_boxplot() +
    labs(
      title = paste("Predictive R² for", outcome_name),
      subtitle = paste0(n_reps, " repetitions × ", n_folds, " folds = ", n_reps*n_folds, " values per model"),
      x = "Predictor", 
      y = "Predictive R²"
    ) +
    theme_light() + theme(legend.position = "none")
  
  p2 <- ggplot(plot_data, aes(x = Model, y = RMSE, fill = Model)) +
    geom_boxplot() +
    labs(
      title = paste("RMSE for", outcome_name), 
      subtitle = paste0(n_reps, " repetitions × ", n_folds, " folds = ", n_reps*n_folds, " values per model"),
      x = "Predictor", 
      y = "RMSE"
    ) +
    theme_light() + theme(legend.position = "none")
  
  print(p1 + p2)
}

################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# remove NA values
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# Perform repeated cross-validation with predictive R² calculated on test sets
cv_slope_results <- repeated_cross_validate_mixed_models(
  data_slope, fixed_model, random_model, slope_outcome, 
  predictors, n_folds, n_repetitions
)

cv_amp_results <- repeated_cross_validate_mixed_models(
  data_amp, fixed_model, random_model, amplitude_outcome, 
  predictors, n_folds, n_repetitions
)

# Run bootstrap comparisons
message("Performing bootstrap comparisons for Sleep Slope...")
slope_comparisons <- compare_to_best_model(
  cv_slope_results, 
  n_bootstrap = n_bootstrap, 
  conf_level = conf_level
)

message("Performing bootstrap comparisons for Sleep Amplitude...")
amp_comparisons <- compare_to_best_model(
  cv_amp_results, 
  n_bootstrap = n_bootstrap, 
  conf_level = conf_level
)

# Calculate effect sizes compared to best AIC model
slope_effect_sizes <- calculate_effect_sizes(data_slope, slope_outcome, cv_slope_results, predictors)
amp_effect_sizes <- calculate_effect_sizes(data_amp, amplitude_outcome, cv_amp_results, predictors)

# Final model tables with bootstrap results and BIC
slope_models <- fit_final_models(data_slope, slope_outcome, "Sleep Slope", slope_comparisons, cv_slope_results, slope_effect_sizes, predictors)
amp_models <- fit_final_models(data_amp, amplitude_outcome, "Sleep Amplitude", amp_comparisons, cv_amp_results, amp_effect_sizes, predictors)

# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)

# Create and display R² boxplots with repetition information
slope_r2_plot <- plot_r2_boxplots_with_counts(cv_slope_results, "Sleep Slope", predictors)
amplitude_r2_plot <- plot_r2_boxplots_with_counts(cv_amp_results, "Sleep Amplitude", predictors)

# Add bootstrap visualization
plot_bootstrap_diffs <- function(comparisons, outcome_name) {
  if (nrow(comparisons$comparisons) == 0) return(NULL)
  
  plot_data <- comparisons$comparisons
  
  p <- ggplot(plot_data, aes(x = Compared_Model, y = Mean_R2_Diff)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Bootstrap confidence intervals for difference in R² -", outcome_name),
      subtitle = paste("Best model:", comparisons$best_model, "minus other models"),
      x = "Compared Model",
      y = "Difference in R²"
    ) +
    theme_light()
  
  print(p)
}

message("Creating bootstrap visualization plots...")
slope_bootstrap_plot <- plot_bootstrap_diffs(slope_comparisons, "Sleep Slope")
amp_bootstrap_plot <- plot_bootstrap_diffs(amp_comparisons, "Sleep Amplitude")

# Save results for further analysis if needed
save(cv_slope_results, cv_amp_results, slope_comparisons, amp_comparisons,
     slope_effect_sizes, amp_effect_sizes, slope_models, amp_models,
     file = "repeated_cv_results.RData")