library(dplyr)
library(ggplot2)
library(tidyr)


data <- read.csv("WakeSleepAllData.csv")

## Initial data exploration # CLAUDE
cat("Number of unique participants:", length(unique(data$Participant)), "\n")
cat("Number of unique sessions:", length(unique(paste(data$Participant, data$Session))), "\n")

# remove NA values
data <- data %>% filter(!is.na(Sleep_Slope_Matched))
data <- data %>% filter(!is.na(Sleep_Amplitude))


set.seed(123)
n_boot <- 1000
participants <- unique(data$Participant)
boot_results <- data.frame(Model = character(), Rsquared = numeric(), Iteration = integer())

for (i in 1:n_boot) {
  # Sample participants with replacement
  sampled_participants <- sample(participants, length(participants), replace = TRUE)
  
  # Collect all their data (full rows per participant)
  boot_data <- do.call(rbind, lapply(sampled_participants, function(p) {
    data[data$Participant == p, ]
  }))
  
  # Fit models
  m1 <- summary(lm(Sleep_Slope_Matched ~ Age + Amplitude, data = boot_data))
  m2 <- summary(lm(Sleep_Slope_Matched ~ Age + Duration, data = boot_data))
  m3 <- summary(lm(Sleep_Slope_Matched ~ Age + Offset, data = boot_data))
  m4 <- summary(lm(Sleep_Slope_Matched ~ Age + Exponent, data = boot_data))
  
  # Store adjusted R² values
  boot_results <- rbind(boot_results,
                        data.frame(Model = "Amplitude", Rsquared = m1$adj.r.squared, Iteration = i),
                        data.frame(Model = "Duration", Rsquared = m2$adj.r.squared, Iteration = i),
                        data.frame(Model = "Offset", Rsquared = m3$adj.r.squared, Iteration = i),
                        data.frame(Model = "Exponent", Rsquared = m4$adj.r.squared, Iteration = i))
}

# Summarize with confidence intervals
ci_summary <- boot_results %>%
  group_by(Model) %>%
  summarise(
    mean_r2 = mean(Rsquared),
    lower_95 = quantile(Rsquared, 0.025),
    upper_95 = quantile(Rsquared, 0.975)
  )

print(ci_summary)


ggplot(ci_summary, aes(x = Model, y = mean_r2)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2) +
  labs(y = "Bootstrapped Adjusted R² (95% CI)",
       title = "Model Comparison with Confidence Intervals") +
  theme_minimal()

pivot_wide <- pivot_wider(boot_results, names_from = Model, values_from = Rsquared)

t.test(pivot_wide$Amplitude, pivot_wide$Duration, paired = TRUE)
t.test(pivot_wide$Amplitude, pivot_wide$Offset, paired = TRUE)
t.test(pivot_wide$Amplitude, pivot_wide$Exponent, paired = TRUE)


