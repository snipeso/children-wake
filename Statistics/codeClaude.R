library(tidyverse)
library(caret)

## Read data
# For demonstration, we're using a small sample but code is ready for the full dataset
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")  # Update to full path if needed

## Initial data exploration
cat("Number of unique participants:", length(unique(data$Participant)), "\n")
cat("Number of unique sessions:", length(unique(paste(data$Participant, data$Session))), "\n")

## Process data

# Initial models to compare predictors
f1 <- summary(lm(Sleep_Slope_Matched ~ Age + Amplitude, data))
f2 <- summary(lm(Sleep_Slope_Matched ~ Age + Duration, data))
f3 <- summary(lm(Sleep_Slope_Matched ~ Age + Exponent, data))
f4 <- summary(lm(Sleep_Slope_Matched ~ Age + Offset, data))

# Show adjusted R-squared values
cat("\nAdjusted R-squared values from initial models:\n")
cat("Age + Amplitude:", f1$adj.r.squared, "\n")
cat("Age + Duration:", f2$adj.r.squared, "\n")
cat("Age + Exponent:", f3$adj.r.squared, "\n")
cat("Age + Offset:", f4$adj.r.squared, "\n\n")

# Remove rows with missing values in the target variable
data <- subset(data, !is.na(Sleep_Slope_Matched))

# Create participant-session ID for proper stratification
data$ParticipantSession <- paste(data$Participant, data$Session, sep = "_")

# Set seed for reproducibility
set.seed(123)

# Define training control with stratification by ParticipantSession
# This ensures all data from the same participant-session stays together
train_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # Number of folds
  classProbs = FALSE,     # Not needed for regression
  index = createFolds(data$ParticipantSession, k = 10, returnTrain = TRUE) # Stratified folds
)

# Train models using cross-validation
model1 <- train(
  Sleep_Slope_Matched ~ Age + Amplitude, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

model2 <- train(
  Sleep_Slope_Matched ~ Age + Duration, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

model3 <- train(
  Sleep_Slope_Matched ~ Age + Offset, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

model4 <- train(
  Sleep_Slope_Matched ~ Age + Exponent, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

# Print model results
cat("\nModel 1 (Age + Amplitude) CV performance:\n")
print(model1)
cat("\nModel 2 (Age + Duration) CV performance:\n")
print(model2)
cat("\nModel 3 (Age + Offset) CV performance:\n")
print(model3)
cat("\nModel 4 (Age + Exponent) CV performance:\n")
print(model4)

# Combine all models for comparison
resamps <- resamples(list(
  "Age + Amplitude" = model1, 
  "Age + Duration" = model2,
  "Age + Offset" = model3, 
  "Age + Exponent" = model4
))

# Summary of cross-validation results
summary(resamps)

# Create boxplot to compare models
bwplot(resamps, metric = "Rsquared")

# Alternative: Create boxplots of CV R-squared values using ggplot2 for more control
resamps_df <- as.data.frame(resamps$values)
resamps_long <- pivot_longer(
  resamps_df, 
  cols = contains("Rsquared"), 
  names_to = "Model", 
  values_to = "Rsquared"
)
resamps_long$Model <- gsub("\\.Rsquared", "", resamps_long$Model)

ggplot(resamps_long, aes(x = Model, y = Rsquared)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Cross-Validation R-squared by Model",
       x = "Model",
       y = "R-squared")

# Best model based on highest mean R-squared
best_model_name <- names(which.max(colMeans(resamps$values[, grep("Rsquared", names(resamps$values))])))
cat("\nBest model based on cross-validation R-squared:", gsub("\\.Rsquared", "", best_model_name), "\n")

# Optional: Mixed-effects model to account for participant and session
if(require(lme4)) {
  # This is a more advanced approach that properly accounts for 
  # the nested structure of sessions within participants
  model_mixed <- lmer(Sleep_Slope_Matched ~ Age + Amplitude + (1|Participant/Session), data = data)
  cat("\nMixed effects model results:\n")
  print(summary(model_mixed))
}