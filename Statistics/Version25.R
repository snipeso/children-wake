# Code written by Claude Sonnet 3.7 and Sophia Snipes, 2025

library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models

################################################################################
### Parameters

dataFilepath <- "WakeSleepAllData.csv"

# components of the mixed effects model
outcome_slope <- "Sleep_Slope_Matched"
outcome_amp <- "Sleep_Amplitude"

fixed_model <- "~ Age + "
predictors <- c("Amplitude", "Duration", "Offset", "Exponent") # these get looped into the fixed model
random_model <- "+ (1|Participant)"

# for cross-validation
n_folds <- 10       
n_repetitions <- 10 # set to 10 when running definitively


################################################################################
### functions

### Create participant-based folds with balanced distribution 
# each fold has 90% testing data and 10% training data

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
  
  folds <- list()
  for (i in 1:k) {
    fold_participants <- participant_fold_map$Participant[participant_fold_map$Fold == i]
    test_indices <- which(data$Participant %in% fold_participants)
    train_indices <- which(!data$Participant %in% fold_participants)
    
    folds[[i]] <- list(train = train_indices, test = test_indices)
  }
  return(folds)
}


### Calculate predictive R² for test data (uses "variance explained" r2 to evaluate model performance)

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
    Predictor = character(),
    Predictive_R2 = numeric(),
    RMSE = numeric()
  )
  
  # Run repeated cross-validation
  for (rep in 1:n_repetitions) {
    
    # Create new folds for each repetition with different seed (little hack of indexes as "random" seeds)
    folds <- create_balanced_participant_folds(data, k = n_folds, seed = rep)
    
    for (i in 1:n_folds) {
      train_data <- data[folds[[i]]$train, ]
      test_data  <- data[folds[[i]]$test, ]
      
      for (pred in predictors) {
        
        # Train model on training data
        formula <- as.formula(paste(outcome_var, fixed_model, pred, random_model)) # this assembles SleepMeasure ~ Age + WakeMeasure + (1|Participant)
        model <- lmer(formula, data = train_data)
        
        # Calculate predictive R² metrics on testing data
        r2_metrics <- calculate_prediction_metrics(model, test_data, outcome_var)
        
        # Add to results table
        results <- results %>% add_row(
          Repetition = rep,
          Fold = i,
          Predictor = pred,
          Predictive_R2 = r2_metrics$predictive_r2,
          RMSE = r2_metrics$rmse
        )
      }
    }
  }
  
  # Summarize results by model
  summary_results <- results %>%
    group_by(Predictor) %>%
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


### Calculate AIC and BIC for models
calculate_model_metrics <- function(data, fixed_model, random_model, outcome_var, predictors) {
  
  # Initialize results dataframe
  metrics_results <- data.frame(
    Predictor = character(),
    AIC = numeric(),
    BIC = numeric()
  )
  
  for (pred in predictors) {
    # Create formula and fit model
    formula <- as.formula(paste(outcome_var, fixed_model, pred, random_model))
    model <- lmer(formula, data = data)
    
    # Calculate metrics
    model_aic <- AIC(model)
    model_bic <- BIC(model)
    
    # Add to results table
    metrics_results <- metrics_results %>% add_row(
      Predictor = pred,
      AIC = model_aic,
      BIC = model_bic
    )
  }
  
  return(metrics_results)
}

### assemble information on model comparison in table - modified version
assemble_final_models <- function(outcome_name, metrics_results, cv_results) {
  
  # Merge the AIC/BIC results with the cross-validation results
  comparison_table <- metrics_results %>%
    left_join(cv_results$summary, by = "Predictor") %>%
    mutate(Outcome = outcome_name) %>%
    select(Outcome, Predictor, AIC, BIC, Mean_Predictive_R2, SD_Predictive_R2, Mean_RMSE, SD_RMSE)
  
  # Return sorted by AIC
  return(comparison_table %>% arrange(AIC))
}

# plot boxoplots
plot_r2_boxplots <- function(cv_results, outcome_name, predictors) {
  plot_data <- cv_results$fold_results
  boxplot(Predictive_R2 ~ Predictor, data = plot_data,
          main = paste("R² for", outcome_name),
          ylab = "Predictive R²")
}


################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# Scramble data to check what it looks like when random (quality control)
#  set.seed(48)
# 
#  data <- data %>% mutate(
#  Amplitude = sample(Amplitude),
#  Duration = sample(Duration),
#  Offset = sample(Offset),
#  Exponent = sample(Exponent)
# )
# 
# # Important: reset the seed after randomization
#  set.seed(NULL)

# remove NA values
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# Perform repeated cross-validation with predictive R² calculated on test sets
cv_slope_results <- repeated_cross_validate_mixed_models(
  data_slope, fixed_model, random_model, outcome_slope, 
  predictors, n_folds, n_repetitions
)

cv_amp_results <- repeated_cross_validate_mixed_models(
  data_amp, fixed_model, random_model, outcome_amp, 
  predictors, n_folds, n_repetitions
)

# Calculate AIC and BIC
slope_metrics <- calculate_model_metrics(data_slope, fixed_model, random_model, outcome_slope, predictors)
amp_metrics <- calculate_model_metrics(data_amp, fixed_model, random_model, outcome_amp, predictors)

# Final model tables
slope_models <- assemble_final_models("Sleep Slope", slope_metrics, cv_slope_results)
amp_models <- assemble_final_models("Sleep Amplitude", amp_metrics, cv_amp_results)


# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)

# Create and display R² boxplots with repetition information
slope_r2_plot <- plot_r2_boxplots(cv_slope_results, "Sleep Slope", predictors)
amplitude_r2_plot <- plot_r2_boxplots(cv_amp_results, "Sleep Amplitude", predictors)
