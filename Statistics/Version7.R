library(tidyverse)
library(lme4)       # For mixed effects models
library(lmerTest)   # For p-values in mixed models
library(MuMIn)      # For R-squared calculation with mixed models

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Subset data to remove NA values
data_slope <- subset(data, !is.na(Sleep_Slope_Matched))
data_amp <- subset(data, !is.na(Sleep_Amplitude))

# ------ CROSS-VALIDATION APPROACH ------

# Function to perform k-fold cross-validation for mixed-effects models
cross_validate_models <- function(data, outcome_var, k=10) {
  # Create folds
  set.seed(123)
  folds <- sample(1:k, nrow(data), replace=TRUE)
  
  # Create empty matrix to store R-squared values
  me_rsquared <- matrix(NA, nrow=k, ncol=4)
  
  for (i in 1:k) {
    # Split data into training and testing
    train_indices <- which(folds != i)
    test_indices <- which(folds == i)
    
    train <- data[train_indices, ]
    test <- data[test_indices, ]
    
    # For mixed-effects models
    me_formula1 <- as.formula(paste(outcome_var, "~ Age + Amplitude + (1|Participant)"))
    me_formula2 <- as.formula(paste(outcome_var, "~ Age + Duration + (1|Participant)"))
    me_formula3 <- as.formula(paste(outcome_var, "~ Age + Offset + (1|Participant)"))
    me_formula4 <- as.formula(paste(outcome_var, "~ Age + Exponent + (1|Participant)"))
    
    me1 <- lmer(me_formula1, data=train)
    me2 <- lmer(me_formula2, data=train)
    me3 <- lmer(me_formula3, data=train)
    me4 <- lmer(me_formula4, data=train)
    
    # Make predictions
    me1_pred <- predict(me1, newdata=test, allow.new.levels=TRUE)
    me2_pred <- predict(me2, newdata=test, allow.new.levels=TRUE)
    me3_pred <- predict(me3, newdata=test, allow.new.levels=TRUE)
    me4_pred <- predict(me4, newdata=test, allow.new.levels=TRUE)
    
    # Calculate R-squared for each model
    actual <- test[[outcome_var]]
    mean_actual <- mean(actual)
    
    # For mixed-effects models
    me_rsquared[i, 1] <- 1 - sum((actual - me1_pred)^2) / sum((actual - mean_actual)^2)
    me_rsquared[i, 2] <- 1 - sum((actual - me2_pred)^2) / sum((actual - mean_actual)^2)
    me_rsquared[i, 3] <- 1 - sum((actual - me3_pred)^2) / sum((actual - mean_actual)^2)
    me_rsquared[i, 4] <- 1 - sum((actual - me4_pred)^2) / sum((actual - mean_actual)^2)
  }
  
  colnames(me_rsquared) <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  return(list(me=me_rsquared))
}

# Perform cross-validation
cat("\nPerforming cross-validation...\n")
cv_slope <- cross_validate_models(data_slope, "Sleep_Slope_Matched")
cv_amp <- cross_validate_models(data_amp, "Sleep_Amplitude")

# Create boxplots - only for mixed effects models
par(mfrow=c(2,1))

# Slope - Mixed Effects (note: these are conditional R² values)
boxplot(cv_slope$me, 
        main="Sleep Slope - Mixed Effects Models\n(Conditional R²)",
        ylab="Cross-Validated R²",
        col="skyblue",
        border="darkblue")
points(colMeans(cv_slope$me), col="red", pch=19)
text(1:4, colMeans(cv_slope$me) + 0.03, labels=round(colMeans(cv_slope$me), 3), col="red")

# Amplitude - Mixed Effects (note: these are conditional R² values)
boxplot(cv_amp$me, 
        main="Sleep Amplitude - Mixed Effects Models\n(Conditional R²)",
        ylab="Cross-Validated R²",
        col="palegreen",
        border="darkgreen")
points(colMeans(cv_amp$me), col="red", pch=19)
text(1:4, colMeans(cv_amp$me) + 0.03, labels=round(colMeans(cv_amp$me), 3), col="red")

# ------ SUMMARIZE RESULTS ------

# Function to report results and identify best predictor
report_results <- function(cv_results, title) {
  cat("\n===== SUMMARY OF", title, "RESULTS =====\n")
  
  # Calculate mean R-squared values
  mean_rsq <- colMeans(cv_results$me)
  
  # Print each predictor's R-squared value
  for (i in 1:length(mean_rsq)) {
    cat(names(mean_rsq)[i], ": ", round(mean_rsq[i], 4),\w "\n")
  }
  
  # Find the best predictor
  best_predictor <- names(mean_rsq)[which.max(mean_rsq)]
  cat("\nBest predictor (highest cross-validated R²):", best_predictor, "\n")
  
  return(mean_rsq)
}

# Create summaries
slope_summary <- report_results(cv_slope, "SLEEP SLOPE")
amp_summary <- report_results(cv_amp, "SLEEP AMPLITUDE")

# Print a final conclusion
cat("\n===== FINAL CONCLUSION =====\n")
cat("Best wake predictor for Sleep Slope: ", names(slope_summary)[which.max(slope_summary)], "\n")
cat("Best wake predictor for Sleep Amplitude: ", names(amp_summary)[which.max(amp_summary)], "\n")