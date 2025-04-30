library(tidyverse)
library(lme4)       # For mixed effects models
library(lmerTest)   # For p-values in mixed models
library(caret)      # For cross-validation
library(MuMIn)      # For AIC calculation with mixed models

# Read data - replace with your actual file path
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Create participant-session ID for stratified cross-validation
data$ParticipantSession <- paste(data$Participant, data$Session, sep = "_")

# Remove rows with missing outcome variables
data_slope <- subset(data, !is.na(Sleep_Slope_Matched))
data_amplitude <- subset(data, !is.na(Sleep_Amplitude_Matched))

#=====================================================================
# ANALYSIS FOR SLEEP SLOPE
#=====================================================================

# Define cross-validation control with stratification by participant-session
set.seed(123)
train_control <- trainControl(
  method = "cv",
  number = 10,
  index = createFolds(data_slope$ParticipantSession, k = 10, returnTrain = TRUE)
)

# Function to train and evaluate mixed-effects models
fit_mixed_model <- function(formula, data, train_indices) {
  # Fit the model on training data
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  model <- lmer(formula, data = train_data, REML = FALSE)
  
  # Calculate RMSE and R² on test data
  predictions <- predict(model, newdata = test_data, allow.new.levels = TRUE)
  rmse <- sqrt(mean((test_data$Sleep_Slope_Matched - predictions)^2, na.rm = TRUE))
  
  # Calculate R² (1 - SSE/SST)
  ss_total <- sum((test_data$Sleep_Slope_Matched - mean(test_data$Sleep_Slope_Matched))^2, na.rm = TRUE)
  ss_error <- sum((test_data$Sleep_Slope_Matched - predictions)^2, na.rm = TRUE)
  r_squared <- 1 - ss_error/ss_total
  
  return(list(RMSE = rmse, Rsquared = r_squared))
}

# Create formulas for the 4 models
formulas_slope <- list(
  m1 = Sleep_Slope_Matched ~ Age + Amplitude + (1|Participant),
  m2 = Sleep_Slope_Matched ~ Age + Duration + (1|Participant),
  m3 = Sleep_Slope_Matched ~ Age + Offset + (1|Participant),
  m4 = Sleep_Slope_Matched ~ Age + Exponent + (1|Participant)
)

# Run cross-validation for all models
cv_results_slope <- list()
for (fold in 1:length(train_control$index)) {
  fold_results <- lapply(formulas_slope, function(formula) {
    fit_mixed_model(formula, data_slope, train_control$index[[fold]])
  })
  cv_results_slope[[fold]] <- fold_results
}

# Extract R-squared values across all folds for all models
cv_rsq_slope <- data.frame(
  Model1 = sapply(cv_results_slope, function(x) x$m1$Rsquared),
  Model2 = sapply(cv_results_slope, function(x) x$m2$Rsquared),
  Model3 = sapply(cv_results_slope, function(x) x$m3$Rsquared),
  Model4 = sapply(cv_results_slope, function(x) x$m4$Rsquared)
)
colnames(cv_rsq_slope) <- c("Age+Amplitude", "Age+Duration", "Age+Offset", "Age+Exponent")

# Fit full models on all data for AIC comparison
full_models_slope <- list(
  Model1 = lmer(formulas_slope$m1, data = data_slope, REML = FALSE),
  Model2 = lmer(formulas_slope$m2, data = data_slope, REML = FALSE),
  Model3 = lmer(formulas_slope$m3, data = data_slope, REML = FALSE),
  Model4 = lmer(formulas_slope$m4, data = data_slope, REML = FALSE)
)

# Calculate AIC values
AIC_values_slope <- sapply(full_models_slope, AIC)
BIC_values_slope <- sapply(full_models_slope, BIC)

#=====================================================================
# ANALYSIS FOR SLEEP AMPLITUDE
#=====================================================================

# The same process for sleep amplitude
formulas_amp <- list(
  m1 = Sleep_Amplitude_Matched ~ Age + Amplitude + (1|Participant),
  m2 = Sleep_Amplitude_Matched ~ Age + Duration + (1|Participant),
  m3 = Sleep_Amplitude_Matched ~ Age + Offset + (1|Participant),
  m4 = Sleep_Amplitude_Matched ~ Age + Exponent + (1|Participant)
)

# Run cross-validation for all models
cv_results_amp <- list()
for (fold in 1:length(train_control$index)) {
  fold_results <- lapply(formulas_amp, function(formula) {
    fit_mixed_model(formula, data_amplitude, train_control$index[[fold]])
  })
  cv_results_amp[[fold]] <- fold_results
}

# Extract R-squared values across all folds for all models
cv_rsq_amp <- data.frame(
  Model1 = sapply(cv_results_amp, function(x) x$m1$Rsquared),
  Model2 = sapply(cv_results_amp, function(x) x$m2$Rsquared),
  Model3 = sapply(cv_results_amp, function(x) x$m3$Rsquared),
  Model4 = sapply(cv_results_amp, function(x) x$m4$Rsquared)
)
colnames(cv_rsq_amp) <- c("Age+Amplitude", "Age+Duration", "Age+Offset", "Age+Exponent")

# Fit full models on all data for AIC comparison
full_models_amp <- list(
  Model1 = lmer(formulas_amp$m1, data = data_amplitude, REML = FALSE),
  Model2 = lmer(formulas_amp$m2, data = data_amplitude, REML = FALSE),
  Model3 = lmer(formulas_amp$m3, data = data_amplitude, REML = FALSE),
  Model4 = lmer(formulas_amp$m4, data = data_amplitude, REML = FALSE)
)

# Calculate AIC values
AIC_values_amp <- sapply(full_models_amp, AIC)
BIC_values_amp <- sapply(full_models_amp, BIC)

#=====================================================================
# RESULTS AND STATISTICAL COMPARISONS
#=====================================================================

# Print results for Sleep Slope
cat("============= SLEEP SLOPE RESULTS =============\n")
cat("AIC values (lower is better):\n")
print(AIC_values_slope)
cat("\nBest model by AIC:", names(which.min(AIC_values_slope)), "\n\n")

cat("BIC values (lower is better):\n")
print(BIC_values_slope)
cat("\nBest model by BIC:", names(which.min(BIC_values_slope)), "\n\n")

cat("Cross-validation R-squared values summary:\n")
print(summary(cv_rsq_slope))

# Perform paired t-tests between the best model and others
best_model_idx <- which.max(colMeans(cv_rsq_slope))
best_model_name <- colnames(cv_rsq_slope)[best_model_idx]

cat("\nPaired t-tests comparing best model (", best_model_name, ") with other models:\n")
for (i in 1:ncol(cv_rsq_slope)) {
  if (i != best_model_idx) {
    test_result <- t.test(cv_rsq_slope[, best_model_idx], cv_rsq_slope[, i], paired=TRUE)
    cat("vs", colnames(cv_rsq_slope)[i], "p-value:", round(test_result$p.value, 4), 
        "95% CI:", round(test_result$conf.int[1], 4), "to", round(test_result$conf.int[2], 4), "\n")
    
    # Interpret results
    if (test_result$p.value < 0.05) {
      if (test_result$conf.int[1] > 0) {
        cat("  ", best_model_name, "is significantly better\n")
      } else {
        cat("  ", colnames(cv_rsq_slope)[i], "is significantly better\n")
      }
    } else {
      cat("  No significant difference\n")
    }
  }
}

# Print results for Sleep Amplitude
cat("\n\n============= SLEEP AMPLITUDE RESULTS =============\n")
cat("AIC values (lower is better):\n")
print(AIC_values_amp)
cat("\nBest model by AIC:", names(which.min(AIC_values_amp)), "\n\n")

cat("BIC values (lower is better):\n")
print(BIC_values_amp)
cat("\nBest model by BIC:", names(which.min(BIC_values_amp)), "\n\n")

cat("Cross-validation R-squared values summary:\n")
print(summary(cv_rsq_amp))

# Perform paired t-tests between the best model and others
best_model_idx <- which.max(colMeans(cv_rsq_amp))
best_model_name <- colnames(cv_rsq_amp)[best_model_idx]

cat("\nPaired t-tests comparing best model (", best_model_name, ") with other models:\n")
for (i in 1:ncol(cv_rsq_amp)) {
  if (i != best_model_idx) {
    test_result <- t.test(cv_rsq_amp[, best_model_idx], cv_rsq_amp[, i], paired=TRUE)
    cat("vs", colnames(cv_rsq_amp)[i], "p-value:", round(test_result$p.value, 4), 
        "95% CI:", round(test_result$conf.int[1], 4), "to", round(test_result$conf.int[2], 4), "\n")
    
    # Interpret results
    if (test_result$p.value < 0.05) {
      if (test_result$conf.int[1] > 0) {
        cat("  ", best_model_name, "is significantly better\n")
      } else {
        cat("  ", colnames(cv_rsq_amp)[i], "is significantly better\n")
      }
    } else {
      cat("  No significant difference\n")
    }
  }
}