library(tidyverse)
library(lme4)       # For mixed effects models
library(lmerTest)   # For p-values in mixed models
library(MuMIn)      # For R-squared calculation with mixed models

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Subset data to remove NA values
data_slope <- subset(data, !is.na(Sleep_Slope_Matched))
data_amp <- subset(data, !is.na(Sleep_Amplitude_Matched))

# ------ SIMPLE LINEAR MODELS (like the original code) ------

# For Sleep Slope
cat("===== SIMPLE LINEAR MODELS FOR SLEEP SLOPE =====\n")
f1 <- summary(lm(Sleep_Slope_Matched ~ Age + Amplitude, data_slope))
f2 <- summary(lm(Sleep_Slope_Matched ~ Age + Duration, data_slope))
f3 <- summary(lm(Sleep_Slope_Matched ~ Age + Offset, data_slope))
f4 <- summary(lm(Sleep_Slope_Matched ~ Age + Exponent, data_slope))

# Create a data frame for easy comparison
lm_rsq_slope <- data.frame(
  Model = c("Age + Amplitude", "Age + Duration", "Age + Offset", "Age + Exponent"),
  Adj_R_squared = c(f1$adj.r.squared, f2$adj.r.squared, f3$adj.r.squared, f4$adj.r.squared)
)

# Sort by R-squared
lm_rsq_slope <- lm_rsq_slope[order(-lm_rsq_slope$Adj_R_squared),]
print(lm_rsq_slope)

# For Sleep Amplitude
cat("\n===== SIMPLE LINEAR MODELS FOR SLEEP AMPLITUDE =====\n")
a1 <- summary(lm(Sleep_Amplitude_Matched ~ Age + Amplitude, data_amp))
a2 <- summary(lm(Sleep_Amplitude_Matched ~ Age + Duration, data_amp))
a3 <- summary(lm(Sleep_Amplitude_Matched ~ Age + Offset, data_amp))
a4 <- summary(lm(Sleep_Amplitude_Matched ~ Age + Exponent, data_amp))

# Create a data frame for easy comparison
lm_rsq_amp <- data.frame(
  Model = c("Age + Amplitude", "Age + Duration", "Age + Offset", "Age + Exponent"),
  Adj_R_squared = c(a1$adj.r.squared, a2$adj.r.squared, a3$adj.r.squared, a4$adj.r.squared)
)

# Sort by R-squared
lm_rsq_amp <- lm_rsq_amp[order(-lm_rsq_amp$Adj_R_squared),]
print(lm_rsq_amp)

# ------ MIXED EFFECTS MODELS ------

# For Sleep Slope
cat("\n===== MIXED EFFECTS MODELS FOR SLEEP SLOPE =====\n")
m1 <- lmer(Sleep_Slope_Matched ~ Age + Amplitude + (1|Participant), data_slope, REML = FALSE)
m2 <- lmer(Sleep_Slope_Matched ~ Age + Duration + (1|Participant), data_slope, REML = FALSE)
m3 <- lmer(Sleep_Slope_Matched ~ Age + Offset + (1|Participant), data_slope, REML = FALSE)
m4 <- lmer(Sleep_Slope_Matched ~ Age + Exponent + (1|Participant), data_slope, REML = FALSE)

# Calculate conditional R-squared (variance explained by fixed + random effects)
# And marginal R-squared (variance explained by fixed effects only)
r2_m1 <- r.squaredGLMM(m1)
r2_m2 <- r.squaredGLMM(m2)
r2_m3 <- r.squaredGLMM(m3)
r2_m4 <- r.squaredGLMM(m4)

# Create a data frame for easy comparison
me_rsq_slope <- data.frame(
  Model = c("Age + Amplitude", "Age + Duration", "Age + Offset", "Age + Exponent"),
  Marginal_R2 = c(r2_m1[1], r2_m2[1], r2_m3[1], r2_m4[1]),  # Fixed effects only
  Conditional_R2 = c(r2_m1[2], r2_m2[2], r2_m3[2], r2_m4[2])  # Fixed + random effects
)

# Sort by marginal R-squared (equivalent to fixed effects)
me_rsq_slope <- me_rsq_slope[order(-me_rsq_slope$Marginal_R2),]
print(me_rsq_slope)

# Calculate AIC for each model
aic_slope <- AIC(m1, m2, m3, m4)
rownames(aic_slope) <- c("Age + Amplitude", "Age + Duration", "Age + Offset", "Age + Exponent")
cat("\nAIC values for Sleep Slope models (lower is better):\n")
print(aic_slope[order(aic_slope$AIC),])

# For Sleep Amplitude
cat("\n===== MIXED EFFECTS MODELS FOR SLEEP AMPLITUDE =====\n")
am1 <- lmer(Sleep_Amplitude_Matched ~ Age + Amplitude + (1|Participant), data_amp, REML = FALSE)
am2 <- lmer(Sleep_Amplitude_Matched ~ Age + Duration + (1|Participant), data_amp, REML = FALSE)
am3 <- lmer(Sleep_Amplitude_Matched ~ Age + Offset + (1|Participant), data_amp, REML = FALSE)
am4 <- lmer(Sleep_Amplitude_Matched ~ Age + Exponent + (1|Participant), data_amp, REML = FALSE)

# Calculate R-squared values
r2_am1 <- r.squaredGLMM(am1)
r2_am2 <- r.squaredGLMM(am2)
r2_am3 <- r.squaredGLMM(am3)
r2_am4 <- r.squaredGLMM(am4)

# Create a data frame for easy comparison
me_rsq_amp <- data.frame(
  Model = c("Age + Amplitude", "Age + Duration", "Age + Offset", "Age + Exponent"),
  Marginal_R2 = c(r2_am1[1], r2_am2[1], r2_am3[1], r2_am4[1]),
  Conditional_R2 = c(r2_am1[2], r2_am2[2], r2_am3[2], r2_am4[2])
)

# Sort by marginal R-squared
me_rsq_amp <- me_rsq_amp[order(-me_rsq_amp$Marginal_R2),]
print(me_rsq_amp)

# Calculate AIC for each model
aic_amp <- AIC(am1, am2, am3, am4)
rownames(aic_amp) <- c("Age + Amplitude", "Age + Duration", "Age + Offset", "Age + Exponent")
cat("\nAIC values for Sleep Amplitude models (lower is better):\n")
print(aic_amp[order(aic_amp$AIC),])

# ------ CREATE SIMPLE BOXPLOTS ------

# Create a boxplot comparing adjusted R-squared values from cross-validation
set.seed(123)

# Function to perform k-fold cross-validation for both types of models
cross_validate_models <- function(data, outcome_var, k=10) {
  # Create folds
  folds <- sample(1:k, nrow(data), replace=TRUE)
  
  # Create empty matrices to store R-squared values
  lm_rsquared <- matrix(NA, nrow=k, ncol=4)
  me_rsquared <- matrix(NA, nrow=k, ncol=4)
  
  for (i in 1:k) {
    # Split data into training and testing
    train <- data[folds != i, ]
    test <- data[folds == i, ]
    
    # For linear models
    lm1 <- lm(paste(outcome_var, "~ Age + Amplitude"), data=train)
    lm2 <- lm(paste(outcome_var, "~ Age + Duration"), data=train)
    lm3 <- lm(paste(outcome_var, "~ Age + Offset"), data=train)
    lm4 <- lm(paste(outcome_var, "~ Age + Exponent"), data=train)
    
    # For mixed-effects models
    me1 <- lmer(paste(outcome_var, "~ Age + Amplitude + (1|Participant)"), data=train)
    me2 <- lmer(paste(outcome_var, "~ Age + Duration + (1|Participant)"), data=train)
    me3 <- lmer(paste(outcome_var, "~ Age + Offset + (1|Participant)"), data=train)
    me4 <- lmer(paste(outcome_var, "~ Age + Exponent + (1|Participant)"), data=train)
    
    # Make predictions
    lm1_pred <- predict(lm1, newdata=test)
    lm2_pred <- predict(lm2, newdata=test)
    lm3_pred <- predict(lm3, newdata=test)
    lm4_pred <- predict(lm4, newdata=test)
    
    me1_pred <- predict(me1, newdata=test, allow.new.levels=TRUE)
    me2_pred <- predict(me2, newdata=test, allow.new.levels=TRUE)
    me3_pred <- predict(me3, newdata=test, allow.new.levels=TRUE)
    me4_pred <- predict(me4, newdata=test, allow.new.levels=TRUE)
    
    # Calculate R-squared for each model
    actual <- test[[outcome_var]]
    mean_actual <- mean(actual)
    
    # For linear models
    lm_rsquared[i, 1] <- 1 - sum((actual - lm1_pred)^2) / sum((actual - mean_actual)^2)
    lm_rsquared[i, 2] <- 1 - sum((actual - lm2_pred)^2) / sum((actual - mean_actual)^2)
    lm_rsquared[i, 3] <- 1 - sum((actual - lm3_pred)^2) / sum((actual - mean_actual)^2)
    lm_rsquared[i, 4] <- 1 - sum((actual - lm4_pred)^2) / sum((actual - mean_actual)^2)
    
    # For mixed-effects models
    me_rsquared[i, 1] <- 1 - sum((actual - me1_pred)^2) / sum((actual - mean_actual)^2)
    me_rsquared[i, 2] <- 1 - sum((actual - me2_pred)^2) / sum((actual - mean_actual)^2)
    me_rsquared[i, 3] <- 1 - sum((actual - me3_pred)^2) / sum((actual - mean_actual)^2)
    me_rsquared[i, 4] <- 1 - sum((actual - me4_pred)^2) / sum((actual - mean_actual)^2)
  }
  
  colnames(lm_rsquared) <- c("Amplitude", "Duration", "Offset", "Exponent")
  colnames(me_rsquared) <- c("Amplitude", "Duration", "Offset", "Exponent")
  
  return(list(lm=lm_rsquared, me=me_rsquared))
}

# Perform cross-validation
cv_slope <- cross_validate_models(data_slope, "Sleep_Slope_Matched")
cv_amp <- cross_validate_models(data_amp, "Sleep_Amplitude_Matched")

# Create boxplots
par(mfrow=c(2,2))

# Slope - Linear Models
boxplot(cv_slope$lm, 
        main="Sleep Slope - Linear Models",
        ylab="Cross-Validated R²",
        col="lightblue",
        border="darkblue")
points(colMeans(cv_slope$lm), col="red", pch=19)
text(1:4, colMeans(cv_slope$lm) + 0.03, labels=round(colMeans(cv_slope$lm), 3), col="red")

# Slope - Mixed Effects
boxplot(cv_slope$me, 
        main="Sleep Slope - Mixed Effects",
        ylab="Cross-Validated R²",
        col="skyblue",
        border="darkblue")
points(colMeans(cv_slope$me), col="red", pch=19)
text(1:4, colMeans(cv_slope$me) + 0.03, labels=round(colMeans(cv_slope$me), 3), col="red")

# Amplitude - Linear Models
boxplot(cv_amp$lm, 
        main="Sleep Amplitude - Linear Models",
        ylab="Cross-Validated R²",
        col="lightgreen",
        border="darkgreen")
points(colMeans(cv_amp$lm), col="red", pch=19)
text(1:4, colMeans(cv_amp$lm) + 0.03, labels=round(colMeans(cv_amp$lm), 3), col="red")

# Amplitude - Mixed Effects
boxplot(cv_amp$me, 
        main="Sleep Amplitude - Mixed Effects",
        ylab="Cross-Validated R²",
        col="palegreen",
        border="darkgreen")
points(colMeans(cv_amp$me), col="red", pch=19)
text(1:4, colMeans(cv_amp$me) + 0.03, labels=round(colMeans(cv_amp$me), 3), col="red")