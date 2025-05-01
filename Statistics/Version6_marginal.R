library(tidyverse)
library(lme4)
library(MuMIn)
library(ggplot2)
library(reshape2)
library(lmerTest)

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Subset data to remove NA values
data_slope <- subset(data, !is.na(Sleep_Slope_Matched))
data_amp <- subset(data, !is.na(Sleep_Amplitude))

# Function to get both marginal and conditional R² values from mixed-effects models
get_r2_values <- function(data, outcome_var, repetitions=100) {
  predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  marginal_r2 <- matrix(NA, nrow=repetitions, ncol=4)
  conditional_r2 <- matrix(NA, nrow=repetitions, ncol=4)
  colnames(marginal_r2) <- colnames(conditional_r2) <- predictors
  
  for (i in 1:repetitions) {
    # Create bootstrap sample
    set.seed(123 + i)
    sampled_participants <- sample(unique(data$Participant), replace=TRUE)
    bootstrap_indices <- c()
    
    for (p in sampled_participants) {
      participant_indices <- which(data$Participant == p)
      bootstrap_indices <- c(bootstrap_indices, participant_indices)
    }
    
    boot_data <- data[bootstrap_indices, ]
    
    # Fit models for each predictor
    for (j in 1:length(predictors)) {
      formula <- as.formula(paste0(outcome_var, " ~ Age + ", predictors[j], " + (1|Participant)"))
      model <- lmer(formula, data=boot_data, REML=FALSE)
      r2_values <- r.squaredGLMM(model)
      marginal_r2[i, j] <- r2_values[1]    # Fixed effects only
      conditional_r2[i, j] <- r2_values[2] # Fixed + random effects
    }
  }
  
  return(list(marginal=marginal_r2, conditional=conditional_r2))
}

# Get R² values for both outcome variables
slope_r2 <- get_r2_values(data_slope, "Sleep_Slope_Matched")
amp_r2 <- get_r2_values(data_amp, "Sleep_Amplitude")

# Function to prepare data for plotting
prepare_plot_data <- function(r2_list, outcome) {
  marg_df <- data.frame(r2_list$marginal) %>%
    mutate(Outcome = outcome, R2_Type = "Marginal")
  
  cond_df <- data.frame(r2_list$conditional) %>%
    mutate(Outcome = outcome, R2_Type = "Conditional")
  
  rbind(marg_df, cond_df)
}

# Prepare data for plotting
slope_df <- prepare_plot_data(slope_r2, "Sleep_Slope_Matched")
amp_df <- prepare_plot_data(amp_r2, "Sleep_Amplitude")
all_df <- rbind(slope_df, amp_df)

# Convert to long format for plotting
all_df_long <- melt(all_df, id.vars = c("Outcome", "R2_Type"), 
                    variable.name = "Predictor", value.name = "R2")

# Run Wilcoxon signed-rank tests to compare predictors
run_wilcox_tests <- function(r2_matrix, outcome_name, r2_type) {
  predictors <- colnames(r2_matrix)
  n_pred <- length(predictors)
  results <- data.frame(matrix(NA, nrow=choose(n_pred, 2), ncol=4))
  colnames(results) <- c("Predictor1", "Predictor2", "p.value", "diff")
  
  row <- 1
  for (i in 1:(n_pred - 1)) {
    for (j in (i + 1):n_pred) {
      test <- wilcox.test(r2_matrix[,i], r2_matrix[,j], paired=TRUE)
      results[row, ] <- c(predictors[i], predictors[j], 
                          test$p.value, 
                          mean(r2_matrix[,i]) - mean(r2_matrix[,j]))
      row <- row + 1
    }
  }
  
  results$p.value <- as.numeric(results$p.value)
  results$diff <- as.numeric(results$diff)
  results$significant <- results$p.value < 0.05
  
  cat("\n==== Wilcoxon Tests for", outcome_name, "(", r2_type, "R²) ====\n")
  print(results[order(results$p.value), ])
  
  # Find best predictor
  means <- colMeans(r2_matrix)
  best <- predictors[which.max(means)]
  cat("\nBest predictor for", outcome_name, "(", r2_type, "R²):", best, 
      "(Mean R² =", round(max(means), 4), ")\n")
  
  return(results)
}

# Run tests for marginal R²
cat("\n===== MARGINAL R² COMPARISONS =====\n")
slope_marg_tests <- run_wilcox_tests(slope_r2$marginal, "Sleep Slope", "Marginal")
amp_marg_tests <- run_wilcox_tests(amp_r2$marginal, "Sleep Amplitude", "Marginal")

# Run tests for conditional R²
cat("\n===== CONDITIONAL R² COMPARISONS =====\n")
slope_cond_tests <- run_wilcox_tests(slope_r2$conditional, "Sleep Slope", "Conditional")
amp_cond_tests <- run_wilcox_tests(amp_r2$conditional, "Sleep Amplitude", "Conditional")

# Create boxplot separated by R² type
ggplot(all_df_long, aes(x=Predictor, y=R2, fill=Predictor)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
  facet_grid(R2_Type ~ Outcome, scales="free_y") +
  theme_minimal() +
  labs(
    title = "Comparison of Wake Measures Predicting Sleep Variables",
    subtitle = "Controlling for Age with Participant as random effect",
    x = "Wake Predictor",
    y = "R² Value"
  ) +
  theme(legend.position = "none")
ggsave("wake_sleep_comparison.png", width=10, height=8)

# Also fit and report full models directly
cat("\n===== DIRECT MODEL COMPARISONS =====\n")

# For Sleep Slope
cat("\nSleep Slope Models:\n")
m1 <- lmer(Sleep_Slope_Matched ~ Age + Amplitude + (1|Participant), data_slope, REML = FALSE)
m2 <- lmer(Sleep_Slope_Matched ~ Age + Duration + (1|Participant), data_slope, REML = FALSE)
m3 <- lmer(Sleep_Slope_Matched ~ Age + Offset + (1|Participant), data_slope, REML = FALSE)
m4 <- lmer(Sleep_Slope_Matched ~ Age + Exponent + (1|Participant), data_slope, REML = FALSE)

# Calculate both marginal and conditional R²
direct_slope_marg <- c(
  Amplitude = r.squaredGLMM(m1)[1],
  Duration = r.squaredGLMM(m2)[1],
  Offset = r.squaredGLMM(m3)[1],
  Exponent = r.squaredGLMM(m4)[1]
)

direct_slope_cond <- c(
  Amplitude = r.squaredGLMM(m1)[2],
  Duration = r.squaredGLMM(m2)[2],
  Offset = r.squaredGLMM(m3)[2],
  Exponent = r.squaredGLMM(m4)[2]
)

cat("Marginal R² (fixed effects only):\n")
print(sort(direct_slope_marg, decreasing = TRUE))
cat("\nConditional R² (fixed + random effects):\n")
print(sort(direct_slope_cond, decreasing = TRUE))

# For Sleep Amplitude
cat("\nSleep Amplitude Models:\n")
a1 <- lmer(Sleep_Amplitude ~ Age + Amplitude + (1|Participant), data_amp, REML = FALSE)
a2 <- lmer(Sleep_Amplitude ~ Age + Duration + (1|Participant), data_amp, REML = FALSE)
a3 <- lmer(Sleep_Amplitude ~ Age + Offset + (1|Participant), data_amp, REML = FALSE)
a4 <- lmer(Sleep_Amplitude ~ Age + Exponent + (1|Participant), data_amp, REML = FALSE)

direct_amp_marg <- c(
  Amplitude = r.squaredGLMM(a1)[1],
  Duration = r.squaredGLMM(a2)[1],
  Offset = r.squaredGLMM(a3)[1],
  Exponent = r.squaredGLMM(a4)[1]
)

direct_amp_cond <- c(
  Amplitude = r.squaredGLMM(a1)[2],
  Duration = r.squaredGLMM(a2)[2],
  Offset = r.squaredGLMM(a3)[2],
  Exponent = r.squaredGLMM(a4)[2]
)

cat("Marginal R² (fixed effects only):\n")
print(sort(direct_amp_marg, decreasing = TRUE))
cat("\nConditional R² (fixed + random effects):\n")
print(sort(direct_amp_cond, decreasing = TRUE))

# Print summary of bootstrap and direct model results
cat("\n===== SUMMARY OF RESULTS =====\n")
results_summary <- data.frame(
  Sleep_Slope_Marginal = colMeans(slope_r2$marginal),
  Sleep_Slope_Conditional = colMeans(slope_r2$conditional),
  Sleep_Amplitude_Marginal = colMeans(amp_r2$marginal),
  Sleep_Amplitude_Conditional = colMeans(amp_r2$conditional)
)
print(results_summary)

# Calculate difference between conditional and marginal R²
# (shows variance explained by random effect of Participant)
cat("\nVariance explained by random effect (Participant):\n")
random_effect_var <- data.frame(
  Sleep_Slope = results_summary$Sleep_Slope_Conditional - results_summary$Sleep_Slope_Marginal,
  Sleep_Amplitude = results_summary$Sleep_Amplitude_Conditional - results_summary$Sleep_Amplitude_Marginal
)
print(random_effect_var)