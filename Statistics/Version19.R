# Simplified script for model comparison
# Focuses on cross-validation and bootstrap comparison

library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(MuMIn)         # For R-squared calculation with mixed models
library(effsize)       # For Cohen's d calculation
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

# for cross-validation
n_folds <- 10       
n_repetitions <- 10 # set to 10 when running definitively 

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
    # Create new folds for each repetition with different seed 
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


### Bootstrap-based function for comparing models
### Based directly on the paper's approach, lines 241-298 in your original script

compare_to_best_model <- function(cv_results, n_repetitions, n_bootstrap = 1000, conf_level = 0.95) {
  
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
    Significant = logical()
  )
  
  # Set seed for reproducibility
  set.seed(42)
  
  # For each model comparison
  for (model in models) {
    if (model != best_model) {
      
      # Step 1: Calculate observed difference at repetition level
      # (average across folds to respect dependency structure)
      rep_data <- cv_results$fold_results %>%
        group_by(Repetition, Model) %>%
        summarize(Mean_R2 = mean(Predictive_R2), .groups = "drop") %>%
        filter(Model %in% c(best_model, model)) %>%
        pivot_wider(names_from = Model, values_from = Mean_R2) %>%
        mutate(diff = .data[[best_model]] - .data[[model]]) # calculates the difference between best model and other model
      
      # Get observed mean difference
      obs_diff <- mean(rep_data$diff, na.rm = TRUE)
      
      # Step 2: Bootstrap at the repetition level
      boot_diffs <- numeric(n_bootstrap)
      
      # Corrected bootstrapping - sample at repetition level, not individual observation level
      n_rows <- nrow(rep_data)  # Use actual number of rows
      for (i in 1:n_bootstrap) {
        boot_sample <- sample(rep_data$diff, n_rows, replace = TRUE)
        boot_diffs[i] <- mean(boot_sample, na.rm = TRUE)
      }
      
      # Step 3: Calculate confidence interval
      ci <- quantile(boot_diffs, c((1-conf_level)/2, 1-(1-conf_level)/2), na.rm = TRUE)
      
      # Check if the confidence interval includes zero
      significant <- (ci[1] > 0) || (ci[2] < 0)
      
      # Add to results
      comparison_results <- comparison_results %>% add_row(
        Best_Model = best_model,
        Compared_Model = model,
        Mean_R2_Diff = obs_diff,
        CI_Lower = ci[1],
        CI_Upper = ci[2],
        Significant = significant
      )
    }
  }
  
  return(list(
    best_model = best_model,
    comparisons = comparison_results
  ))
}


### Modified fit_final_models function with bootstrapping results

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
    R2_Diff_CI_Lower = numeric(),
    R2_Diff_CI_Upper = numeric(),
    Bootstrap_Significant = logical()
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
    
    # Get bootstrap confidence intervals
    ci_lower <- NA
    ci_upper <- NA
    bootstrap_significant <- NA
    
    if (pred != best_model) {
      bootstrap_row <- bootstrap_comparisons$comparisons %>% 
        filter(Compared_Model == pred)
      if (nrow(bootstrap_row) > 0) {
        ci_lower <- bootstrap_row$CI_Lower
        ci_upper <- bootstrap_row$CI_Upper
        bootstrap_significant <- bootstrap_row$Significant
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
      R2_Diff_CI_Lower = ci_lower,
      R2_Diff_CI_Upper = ci_upper,
      Bootstrap_Significant = bootstrap_significant
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


### Plot functions

# Basic boxplots for R²
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

# Add repetition and fold counts to plot titles
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

# Modified bootstrap visualization function with significance indicators
plot_bootstrap_diffs <- function(comparisons, outcome_name) {
  if (nrow(comparisons$comparisons) == 0) return(NULL)
  
  plot_data <- comparisons$comparisons
  
  p <- ggplot(plot_data, aes(x = Compared_Model, y = Mean_R2_Diff)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    # Add highlight for significant comparisons
    geom_point(data = plot_data %>% filter(Significant), 
               aes(x = Compared_Model, y = Mean_R2_Diff),
               size = 4, shape = 8, color = "red") +
    labs(
      title = paste("Bootstrap confidence intervals for difference in R² -", outcome_name),
      subtitle = paste("Best model:", comparisons$best_model, "minus other models"),
      x = "Compared Model",
      y = "Difference in R²"
    ) +
    theme_light() +
    # Add annotation for interpretation
    annotate("text", x = Inf, y = -Inf, 
             label = "CIs crossing zero suggest no significant difference\nRed stars indicate significant differences", 
             hjust = 1.1, vjust = -1, size = 3, fontface = "italic")
  
  print(p)
}

################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# # Set seed for reproducibility in the randomization
 set.seed(48)
 
 data <- data %>% mutate(
 Amplitude = sample(Amplitude),
 Duration = sample(Duration),
 Offset = sample(Offset),
 Exponent = sample(Exponent)
)
# 
# # Important: reset the seed after randomization
 set.seed(NULL)

# Remove NA values
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# Perform repeated cross-validation with predictive R² calculated on test sets
message("Running cross-validation for Sleep Slope...")
cv_slope_results <- repeated_cross_validate_mixed_models(
  data_slope, fixed_model, random_model, slope_outcome, 
  predictors, n_folds, n_repetitions
)

message("Running cross-validation for Sleep Amplitude...")
cv_amp_results <- repeated_cross_validate_mixed_models(
  data_amp, fixed_model, random_model, amplitude_outcome, 
  predictors, n_folds, n_repetitions
)

# Run bootstrap comparisons
message("Performing bootstrap comparisons for Sleep Slope...")
slope_comparisons <- compare_to_best_model(
  cv_slope_results, n_repetitions, n_bootstrap, conf_level
)

message("Performing bootstrap comparisons for Sleep Amplitude...")
amp_comparisons <- compare_to_best_model(
  cv_amp_results, n_repetitions, n_bootstrap, conf_level
)

# Calculate effect sizes compared to best AIC model
slope_effect_sizes <- calculate_effect_sizes(data_slope, slope_outcome, cv_slope_results, predictors)
amp_effect_sizes <- calculate_effect_sizes(data_amp, amplitude_outcome, cv_amp_results, predictors)

# Final model tables with bootstrap results
slope_models <- fit_final_models(
  data_slope, slope_outcome, "Sleep Slope", 
  slope_comparisons, cv_slope_results, slope_effect_sizes, predictors
)

amp_models <- fit_final_models(
  data_amp, amplitude_outcome, "Sleep Amplitude", 
  amp_comparisons, cv_amp_results, amp_effect_sizes, predictors
)

# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)

# Create a simplified summary table with significance indicators
summary_table <- combined_table %>%
  select(Outcome, Model, Predictive_R2_Mean, Fixed_Effect_p, Bootstrap_Significant) %>%
  mutate(
    Significance = ifelse(Bootstrap_Significant == TRUE, "Significant", "")
  ) %>%
  arrange(Outcome, desc(Predictive_R2_Mean))

print(summary_table)
write.csv(summary_table, "model_significance_summary.csv", row.names = FALSE)

# Create and display R² boxplots with repetition information
message("Creating R² boxplots...")
slope_r2_plot <- plot_r2_boxplots_with_counts(cv_slope_results, "Sleep Slope", predictors)
amplitude_r2_plot <- plot_r2_boxplots_with_counts(cv_amp_results, "Sleep Amplitude", predictors)

# Create bootstrap visualization plots
message("Creating bootstrap visualization plots...")
slope_bootstrap_plot <- plot_bootstrap_diffs(slope_comparisons, "Sleep Slope")
amp_bootstrap_plot <- plot_bootstrap_diffs(amp_comparisons, "Sleep Amplitude")

# Print summary of key findings
message("\n==== Summary of Statistical Comparison Results ====\n")

message("Best predictor for Sleep Slope: ", slope_comparisons$best_model)
message("Significant difference from other predictors? ", 
        ifelse(any(slope_comparisons$comparisons$Significant), "Yes", "No"))

message("\nBest predictor for Sleep Amplitude: ", amp_comparisons$best_model)
message("Significant difference from other predictors? ", 
        ifelse(any(amp_comparisons$comparisons$Significant), "Yes", "No"))

message("\nBootstrap comparison details for Sleep Slope:")
print(slope_comparisons$comparisons %>% select(Compared_Model, Mean_R2_Diff, CI_Lower, CI_Upper, Significant))

message("\nBootstrap comparison details for Sleep Amplitude:")
print(amp_comparisons$comparisons %>% select(Compared_Model, Mean_R2_Diff, CI_Lower, CI_Upper, Significant))

# Save results for further analysis if needed
save(cv_slope_results, cv_amp_results, 
     slope_comparisons, amp_comparisons,
     slope_effect_sizes, amp_effect_sizes, 
     slope_models, amp_models,
     file = "simplified_model_comparison_results.RData")