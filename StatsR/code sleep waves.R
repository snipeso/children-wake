library(tidyverse)
library(caret) #

## Read data

data <- read.csv("C:/Users/mique/Desktop/WakeSleepAllData.csv")

## Process data

# create a boot stratified procedure

## Wake 2 Sleep

f1 <- summary(lm(SWSlope ~ Age + Amplitude, data))
f2 <- summary(lm(SWSlope ~ Age + Duration, data))
f3 <- summary(lm(SWSlope ~ Age + Exponent, data))
f4 <- summary(lm(SWSlope ~ Age + Offset, data))


f1$adj.r.squared
f2$adj.r.squared
f3$adj.r.squared
f4$adj.r.squared


data <- subset(data, !is.na(SWSlope))

# Define training control with stratified resampling
train_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,              # Number of folds
  classProbs = FALSE,      # Not needed for regression
  #sampling = "up",         # Optional: To balance classes (if needed)
  index = createFolds(data$Participant, k = 10, returnTrain = TRUE) # Stratified folds using pre-defined strata
)

# Train a linear regression model
set.seed(123)
model1 <- train(
  SWSlope ~ Age + Amplitude, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

model2 <- train(
  SWSlope ~ Age + Duration, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

model3 <- train(
  SWSlope ~ Age + Offset, # Formula
  data = data,                      # Data
  method = "lm",                   # Linear regression
  trControl = train_control         # Training control
)

model4 <- train(
  SWSlope ~ Age + Exponent, # Formula
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
bwplot(resamps)
## Sleep 2 Wake


