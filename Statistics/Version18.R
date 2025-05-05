# Code adapted from Version16.R, May 2025
# Added proper model comparison techniques based on paper methodology

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

# for cross-validation
n_folds <- 10       
n_repetitions <- 20 # set to 10 when running definitevely

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


### Modified bootstrap comparison method (based on the paper approach)
### This addresses your issue with getting significance for random data

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

### New function for testing permutation-based significance
### This provides an alternative approach to bootstrap for significance testing

test_model_significance <- function(cv_results, n_permutations = 1000, alpha = 0.05) {
  # Find best model based on mean predictive R²
  best_model <- cv_results$summary %>%
    arrange(desc(Mean_Predictive_R2)) %>%
    pull(Model) %>%
    first()
  
  # Extract models from results
  models <- unique(cv_results$fold_results$Model)
  
  # Create results dataframe
  permutation_results <- data.frame(
    Best_Model = character(),
    Compared_Model = character(),
    Observed_Diff = numeric(),
    P_Value = numeric(),
    Significant = logical()
  )
  
  # For each model comparison
  for (model in models) {
    if (model != best_model) {
      # Extract R2 values for both models
      best_r2 <- cv_results$fold_results %>% 
        filter(Model == best_model) %>% 
        pull(Predictive_R2)
      
      model_r2 <- cv_results$fold_results %>% 
        filter(Model == model) %>% 
        pull(Predictive_R2)
      
      # Calculate observed difference
      observed_diff <- mean(best_r2) - mean(model_r2)
      
      # Permutation test
      combined_data <- c(best_r2, model_r2)
      n1 <- length(best_r2)
      n2 <- length(model_r2)
      n_total <- n1 + n2
      
      # Initialize count of permutations exceeding observed diff
      exceed_count <- 0
      
      # Run permutations
      for (i in 1:n_permutations) {
        # Permute the data
        perm_indices <- sample(1:n_total, n_total, replace = FALSE)
        perm_group1 <- combined_data[perm_indices[1:n1]]
        perm_group2 <- combined_data[perm_indices[(n1+1):n_total]]
        
        # Calculate permuted difference
        perm_diff <- mean(perm_group1) - mean(perm_group2)
        
        # Count if permuted diff exceeds observed
        if (abs(perm_diff) >= abs(observed_diff)) {
          exceed_count <- exceed_count + 1
        }
      }
      
      # Calculate p-value
      p_value <- exceed_count / n_permutations
      
      # Add to results
      permutation_results <- permutation_results %>% add_row(
        Best_Model = best_model,
        Compared_Model = model,
        Observed_Diff = observed_diff,
        P_Value = p_value,
        Significant = (p_value <= alpha)
      )
    }
  }
  
  return(list(
    best_model = best_model,
    permutation_results = permutation_results
  ))
}

### New function using Bayesian Information Criterion (BIC) to compare models
### This is based on the approach in the paper

compare_models_with_bic <- function(data, outcome_var, predictors) {
  # Initialize results dataframe
  bic_results <- data.frame(
    Model = character(),
    BIC = numeric(),
    AIC = numeric(),
    LogLik = numeric(),
    Mean_Squared_Error = numeric()
  )
  
  # Fit models and calculate BIC for each predictor
  for (pred in predictors) {
    formula <- as.formula(paste(outcome_var, "~ Age + ", pred, "+ (1|Participant)"))
    model <- lmer(formula, data = data)
    
    # Calculate MSE
    residuals <- residuals(model)
    mse <- mean(residuals^2)
    
    # Add to results
    bic_results <- bic_results %>% add_row(
      Model = pred,
      BIC = BIC(model),
      AIC = AIC(model),
      LogLik = logLik(model),
      Mean_Squared_Error = mse
    )
  }
  
  # Sort by BIC
  bic_results <- bic_results %>% arrange(BIC)
  
  # Calculate BIC differences from best model
  best_bic <- bic_results$BIC[1]
  bic_results$BIC_Diff <- bic_results$BIC - best_bic
  
  # Add interpretation
  bic_results$Evidence <- case_when(
    bic_results$BIC_Diff < 2 ~ "Weak evidence against model",
    bic_results$BIC_Diff < 6 ~ "Positive evidence against model",
    bic_results$BIC_Diff < 10 ~ "Strong evidence against model",
    TRUE ~ "Very strong evidence against model"
  )
  
  return(bic_results)
}

### Feature importance analysis using random forest
### Similar to what was done in Figure 4J of the paper

analyze_feature_importance <- function(data, outcome_var, predictors) {
  library(randomForest)
  
  # Prepare data
  rf_data <- data %>%
    select(Participant, Age, all_of(outcome_var), all_of(predictors)) %>%
    na.omit()
  
  # Train random forest model
  rf_model <- randomForest(
    formula = as.formula(paste(outcome_var, "~", paste(c("Age", predictors), collapse = " + "))),
    data = rf_data,
    importance = TRUE,
    ntree = 500
  )
  
  # Get importance scores
  importance_scores <- importance(rf_model)
  
  # Create summary dataframe
  importance_df <- data.frame(
    Feature = rownames(importance_scores),
    IncMSE = importance_scores[, "%IncMSE"],
    IncNodePurity = importance_scores[, "IncNodePurity"]
  ) %>%
    filter(Feature != "Age") %>% # Remove Age as it's a control variable
    arrange(desc(IncMSE))
  
  # Add percentile ranking
  importance_df$Percentile <- rank(importance_df$IncMSE) / nrow(importance_df) * 100
  
  return(list(
    model = rf_model,
    importance = importance_df
  ))
}

### New AIC-based comparison function

compare_models_with_aic <- function(data, outcome_var, predictors) {
  
  aic_results <- data.frame(
    Model = character(),
    AIC = numeric(),
    LogLik = numeric(),
    Df = numeric()
  )
  
  # Fit models and calculate AIC for each predictor
  for (pred in predictors) {
    formula <- as.formula(paste(outcome_var, "~ Age + ", pred, "+ (1|Participant)"))
    model <- lmer(formula, data = data)
    
    # Extract model info
    model_aic <- AIC(model)
    model_loglik <- as.numeric(logLik(model))
    model_df <- attr(logLik(model), "df")
    
    # Add to results
    aic_results <- aic_results %>% add_row(
      Model = pred,
      AIC = model_aic,
      LogLik = model_loglik,
      Df = model_df
    )
  }
  
  # Sort by AIC
  aic_results <- aic_results %>% arrange(AIC)
  
  # Calculate AIC differences and weights
  min_aic <- min(aic_results$AIC)
  aic_results$delta_AIC <- aic_results$AIC - min_aic
  
  # Calculate AIC weights (probability that the model is the best)
  rel_likelihood <- exp(-0.5 * aic_results$delta_AIC)
  aic_results$AIC_weight <- rel_likelihood / sum(rel_likelihood)
  
  # Add evidence ratio (best model vs current)
  aic_results$Evidence_ratio <- max(aic_results$AIC_weight) / aic_results$AIC_weight
  
  return(aic_results)
}

### Modified fit_final_models function (with p-values and significance flags from bootstrap)

fit_final_models <- function(data, outcome_var, outcome_name, bootstrap_comparisons, permutation_results, cv_results, effect_sizes_info, predictors) {
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
    Bootstrap_Significant = logical(),
    Permutation_p = numeric()
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
    
    # Get permutation p-value
    permutation_p <- NA
    if (pred != best_model) {
      perm_row <- permutation_results$permutation_results %>%
        filter(Compared_Model == pred)
      if (nrow(perm_row) > 0) {
        permutation_p <- perm_row$P_Value
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
      Bootstrap_Significant = bootstrap_significant,
      Permutation_p = permutation_p
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


### Modified plot function to visualize confidence intervals with significance indicators

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

# New plot function for permutation test results
plot_permutation_results <- function(permutation_results, outcome_name) {
  if (nrow(permutation_results$permutation_results) == 0) return(NULL)
  
  plot_data <- permutation_results$permutation_results
  
  p <- ggplot(plot_data, aes(x = Compared_Model, y = Observed_Diff)) +
    geom_col(aes(fill = Significant)) +
    geom_text(aes(label = sprintf("p = %.3f", P_Value), y = Observed_Diff + 0.01 * sign(Observed_Diff)),
              vjust = ifelse(plot_data$Observed_Diff > 0, -0.5, 1.5)) +
    labs(
      title = paste("Permutation test results for difference in R² -", outcome_name),
      subtitle = paste("Best model:", permutation_results$best_model, "vs other models"),
      x = "Compared Model",
      y = "Observed Difference in R²"
    ) +
    scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "grey70")) +
    theme_light() +
    theme(legend.position = "bottom")
  
  print(p)
}

# New function to create a comprehensive visual comparison of models
plot_model_comparison <- function(cv_results, bootstrap_results, permutation_results, bic_results, outcome_name) {
  # Extract data for plotting
  summary_data <- cv_results$summary
  
  # Create performance plot
  p1 <- ggplot(summary_data, aes(x = reorder(Model, -Mean_Predictive_R2), y = Mean_Predictive_R2)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = Mean_Predictive_R2 - SD_Predictive_R2, 
                      ymax = Mean_Predictive_R2 + SD_Predictive_R2), width = 0.2) +
    labs(title = paste("Model Comparison for", outcome_name),
         subtitle = "Predictive R² from cross-validation",
         x = "Wake Parameter",
         y = "Mean Predictive R²") +
    theme_light()
  
  # Create BIC comparison plot
  p2 <- ggplot(bic_results, aes(x = reorder(Model, BIC), y = BIC_Diff)) +
    geom_col(aes(fill = Evidence), alpha = 0.8) +
    scale_fill_manual(values = c(
      "Weak evidence against model" = "darkgreen", 
      "Positive evidence against model" = "orange",
      "Strong evidence against model" = "red",
      "Very strong evidence against model" = "darkred"
    )) +
    labs(title = "BIC Difference from Best Model",
         subtitle = "Lower BIC indicates better model fit",
         x = "Wake Parameter",
         y = "ΔBIC (relative to best)") +
    theme_light() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  # Combine plots
  combined_plot <- p1 + p2 + plot_layout(ncol = 1)
  print(combined_plot)
  
  return(combined_plot)
}

# New function to visualize feature importance from random forest
plot_feature_importance <- function(importance_results, outcome_name) {
  p <- ggplot(importance_results$importance, aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
    geom_col(aes(fill = Percentile), alpha = 0.8) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = paste("Feature Importance for", outcome_name),
         subtitle = "Random Forest - Mean Decrease in MSE",
         x = "Wake Parameter",
         y = "% Increase in MSE when feature is permuted") +
    theme_light() +
    theme(legend.position = "right")
  
  print(p)
  return(p)
}

# Function to generate random data for testing
generate_random_sleep_data <- function(n_participants = 20, n_per_participant = 5, seed = 42) {
  set.seed(seed)
  
  # Generate participant IDs
  participants <- rep(1:n_participants, each = n_per_participant)
  
  # Generate random predictors with no correlation to outcomes
  amplitude <- rnorm(length(participants), mean = 0.5, sd = 0.2)
  duration <- rnorm(length(participants), mean = 8, sd = 1)
  offset <- rnorm(length(participants), mean = 0, sd = 1)
  exponent <- rnorm(length(participants), mean = 1, sd = 0.3)
  
  # Generate random outcomes that are NOT related to predictors
  sleep_slope <- rnorm(length(participants), mean = -0.1, sd = 0.05)
  sleep_amplitude <- rnorm(length(participants), mean = 0.8, sd = 0.3)
  
  # Generate age (control variable)
  age <- sample(18:80, n_participants, replace = TRUE)
  age_expanded <- rep(age, each = n_per_participant)
  
  # Create dataframe
  random_data <- data.frame(
    Participant = participants,
    Age = age_expanded,
    Amplitude = amplitude,
    Duration = duration,
    Offset = offset,
    Exponent = exponent,
    Sleep_Slope_Matched = sleep_slope,
    Sleep_Amplitude = sleep_amplitude
  )
  
  return(random_data)
}

# Function to verify if the statistical comparison methods work correctly
validate_statistical_comparison <- function(n_simulations = 100) {
  # Initialize storage for results
  bootstrap_false_positives <- 0
  permutation_false_positives <- 0
  
  for (i in 1:n_simulations) {
    # Generate random data
    random_data <- generate_random_sleep_data()
    
    # Run cross-validation
    cv_results <- repeated_cross_validate_mixed_models(
      random_data, "~ Age + ", "+ (1|Participant)", "Sleep_Slope_Matched", 
      c("Amplitude", "Duration", "Offset", "Exponent"), 5, 5
    )
    
    # Bootstrap comparison
    bootstrap_comp <- compare_to_best_model(cv_results, 5, 100, 0.95)
    
    # Permutation test
    perm_test <- test_model_significance(cv_results, 100, 0.05)
    
    # Check if any false positives
    if (any(bootstrap_comp$comparisons$Significant)) {
      bootstrap_false_positives <- bootstrap_false_positives + 1
    }
    
    if (any(perm_test$permutation_results$Significant)) {
      permutation_false_positives <- permutation_false_positives + 1
    }
  }
  
  # Calculate false positive rates
  bootstrap_fpr <- bootstrap_false_positives / n_simulations
  permutation_fpr <- permutation_false_positives / n_simulations
  
  # Return results
  return(list(
    bootstrap_false_positive_rate = bootstrap_fpr,
    permutation_false_positive_rate = permutation_fpr,
    expected_false_positive_rate = 0.05,
    n_simulations = n_simulations
  ))
}


################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# For randomization testing (to check if statistical tests work properly)
# Uncomment these lines to run validation on statistical methods
# validation_results <- validate_statistical_comparison(100)
# print(validation_results)

# Set seed for reproducibility in the randomization
# set.seed(48)

# data <- data %>% mutate(
# Amplitude = sample(Amplitude),
# Duration = sample(Duration),
# Offset = sample(Offset),
# Exponent = sample(Exponent)
#)

# Important: reset the seed after randomization
# set.seed(NULL)

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
  cv_slope_results, n_repetitions, n_bootstrap, conf_level
)

message("Performing bootstrap comparisons for Sleep Amplitude...")
amp_comparisons <- compare_to_best_model(
  cv_amp_results, n_repetitions, n_bootstrap, conf_level
)

# Run permutation tests
message("Performing permutation tests for Sleep Slope...")
slope_permutation <- test_model_significance(
  cv_slope_results, 1000, 0.05
)

message("Performing permutation tests for Sleep Amplitude...")
amp_permutation <- test_model_significance(
  cv_amp_results, 1000, 0.05
)

# Calculate effect sizes compared to best AIC model
slope_effect_sizes <- calculate_effect_sizes(data_slope, slope_outcome, cv_slope_results, predictors)
amp_effect_sizes <- calculate_effect_sizes(data_amp, amplitude_outcome, cv_amp_results, predictors)

# BIC-based model comparison
slope_bic <- compare_models_with_bic(data_slope, slope_outcome, predictors)
amp_bic <- compare_models_with_bic(data_amp, amplitude_outcome, predictors)

# AIC-based model comparison
slope_aic <- compare_models_with_aic(data_slope, slope_outcome, predictors)
amp_aic <- compare_models_with_aic(data_amp, amplitude_outcome, predictors)

# Random forest feature importance
slope_importance <- analyze_feature_importance(data_slope, slope_outcome, predictors)
amp_importance <- analyze_feature_importance(data_amp, amplitude_outcome, predictors)

# Final model tables with bootstrap results and BIC
slope_models <- fit_final_models(
  data_slope, slope_outcome, "Sleep Slope", 
  slope_comparisons, slope_permutation, 
  cv_slope_results, slope_effect_sizes, predictors
)

amp_models <- fit_final_models(
  data_amp, amplitude_outcome, "Sleep Amplitude", 
  amp_comparisons, amp_permutation, 
  cv_amp_results, amp_effect_sizes, predictors
)

# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)

# Create and display summary table with significance indicators
summary_table <- combined_table %>%
  select(Outcome, Model, Predictive_R2_Mean, Fixed_Effect_p, Bootstrap_Significant, Permutation_p) %>%
  mutate(
    Significance = case_when(
      Bootstrap_Significant == TRUE ~ "***",
      Permutation_p < 0.05 ~ "**",
      Fixed_Effect_p < 0.05 ~ "*",
      TRUE ~ ""
    )
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

# Create permutation test plots
message("Creating permutation test plots...")
slope_permutation_plot <- plot_permutation_results(slope_permutation, "Sleep Slope")
amp_permutation_plot <- plot_permutation_results(amp_permutation, "Sleep Amplitude")

# Create comprehensive model comparison plots
message("Creating comprehensive model comparison plots...")
slope_comparison_plot <- plot_model_comparison(cv_slope_results, slope_comparisons, slope_permutation, slope_bic, "Sleep Slope")
amp_comparison_plot <- plot_model_comparison(cv_amp_results, amp_comparisons, amp_permutation, amp_bic, "Sleep Amplitude")

# Create feature importance plots
message("Creating feature importance plots...")
slope_importance_plot <- plot_feature_importance(slope_importance, "Sleep Slope")
amp_importance_plot <- plot_feature_importance(amp_importance, "Sleep Amplitude")

# Print summary of key findings
message("\n==== Summary of Statistical Comparison Results ====\n")

message("Best predictor for Sleep Slope: ", slope_comparisons$best_model)
message("Significant difference from other predictors? ", 
        ifelse(any(slope_comparisons$comparisons$Significant), "Yes", "No"))

message("\nBest predictor for Sleep Amplitude: ", amp_comparisons$best_model)
message("Significant difference from other predictors? ", 
        ifelse(any(amp_comparisons$comparisons$Significant), "Yes", "No"))

message("\nRandom Forest Importance Ranking for Sleep Slope:")
print(slope_importance$importance %>% select(Feature, IncMSE) %>% arrange(desc(IncMSE)))

message("\nRandom Forest Importance Ranking for Sleep Amplitude:")
print(amp_importance$importance %>% select(Feature, IncMSE) %>% arrange(desc(IncMSE)))

message("\nBIC Model Comparison for Sleep Slope:")
print(slope_bic %>% select(Model, BIC, BIC_Diff, Evidence))

message("\nBIC Model Comparison for Sleep Amplitude:")
print(amp_bic %>% select(Model, BIC, BIC_Diff, Evidence))

# Save results for further analysis if needed
save(cv_slope_results, cv_amp_results, 
     slope_comparisons, amp_comparisons,
     slope_permutation, amp_permutation,
     slope_effect_sizes, amp_effect_sizes, 
     slope_models, amp_models,
     slope_bic, amp_bic,
     slope_aic, amp_aic,
     slope_importance, amp_importance,
     file = "model_comparison_results.RData")