library(tidyverse)
library(caret) #


## Read data

data <- read.csv("WakeSleepAllData.csv")

## Initial data exploration # CLAUDE
cat("Number of unique participants:", length(unique(data$Participant)), "\n")
cat("Number of unique sessions:", length(unique(paste(data$Participant, data$Session))), "\n")


## Process data

# create a boot stratified procedure

## Wake 2 Sleep

f1 <- summary(lm(Sleep_Slope_Matched ~ Age + Amplitude, data))
f2 <- summary(lm(Sleep_Slope_Matched ~ Age + Duration, data))
f3 <- summary(lm(Sleep_Slope_Matched ~ Age + Exponent, data))
f4 <- summary(lm(Sleep_Slope_Matched ~ Age + Offset, data))


f1$adj.r.squared
f2$adj.r.squared
f3$adj.r.squared
f4$adj.r.squared


data <- subset(data, !is.na(Sleep_Slope_Matched))


# Create participant-session ID for proper stratification # CLAUDE
data$ParticipantSession <- paste(data$Participant, data$Session, sep = "_")

# Define training control with stratified resampling
train_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,              # Number of folds
  classProbs = FALSE,      # Not needed for regression
  #sampling = "up",         # Optional: To balance classes (if needed)
  # index = createFolds(data$Participant, k = 10, returnTrain = TRUE) # Stratified folds using pre-defined strata
  # index = groupKFold(data$Participant, k = 10)
  index = createFolds(data$ParticipantSession, k = 10, returnTrain = TRUE) # Stratified folds
  
)

# Train a linear regression model
set.seed(123)
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


bwplot(model1$resample$Rsquared,model2$resample$Rsquared)
bwplot(model2$resample$Rsquared)
bwplot(model3$resample$Rsquared)
bwplot(model4$resample$Rsquared)

resamps <- resamples(list(Model1 = model1, Model2 = model2,
                          Model3 = model3, Model4 = model4))
bwplot(resamps, metric = "Rsquared")
# bwplot(resamps)
## Sleep 2 Wake


