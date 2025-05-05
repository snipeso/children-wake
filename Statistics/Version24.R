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
n_repetitions <- 5 # set to 10 when running definitively


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
      train <- data[folds[[i]]$train, ]
      test  <- data[folds[[i]]$test, ]
      
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


### assemble information on model comparison in table
assemble_final_models <- function(outcome_name, data, fixed_model, random_model, outcome_var, predictors, cv_results) {
  
  comparison_table <- data.frame(
    Outcome = character(),
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    Predictive_R2_Mean = numeric(),
    Predictive_R2_SD = numeric(),
    RMSE_Mean = numeric(),
    RMSE_SD = numeric()  # Removed trailing comma here
  )
  
  for (pred in predictors) {
    # Get R² statistics from cross-validation
    r2_stats <- cv_results$summary %>%
      filter(Model == pred)
    
    formula <- as.formula(paste(outcome_var, fixed_model, pred, random_model))
    model <- lmer(formula, data = data)
    
    model_aic <- AIC(model)
    model_bic <- BIC(model)
    
    comparison_table <- comparison_table %>% add_row(
      Outcome = outcome_name,
      Model = pred,
      AIC = model_aic,
      BIC = model_bic,
      Predictive_R2_Mean = r2_stats$Mean_Predictive_R2,
      Predictive_R2_SD = r2_stats$SD_Predictive_R2,
      RMSE_Mean = r2_stats$Mean_RMSE,
      RMSE_SD = r2_stats$SD_RMSE  # Removed trailing comma here
    )
  }
  
  return(comparison_table %>% arrange(AIC))
}


# plot boxoplots
plot_r2_boxplots <- function(cv_results, outcome_name, predictors) {
  plot_data <- cv_results$fold_results
  boxplot(Predictive_R2 ~ Model, data = plot_data,
          main = paste("R² for", outcome_name),
          ylab = "Predictive R²")
}


################################################################################
### run

# Read data
data <- read.csv(dataFilepath)

# Set seed for reproducibility in the randomization
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

# Final model tables
slope_models <- assemble_final_models("Sleep Slope", data_slope, fixed_model, random_model, outcome_slope, predictors, cv_slope_results)
amp_models <- assemble_final_models("Sleep Amplitude", data_amp, fixed_model, random_model, outcome_amp, predictors, cv_amp_results)

# Combined final table
combined_table <- rbind(slope_models, amp_models)
print(combined_table)
write.csv(combined_table, "combined_model_comparison.csv", row.names = FALSE)

# Create and display R² boxplots with repetition information
slope_r2_plot <- plot_r2_boxplots(cv_slope_results, "Sleep Slope", predictors)
amplitude_r2_plot <- plot_r2_boxplots(cv_amp_results, "Sleep Amplitude", predictors)
