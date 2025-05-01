# Library loading - consistent use of tidyverse and related packages
library(tidyverse)     # For data manipulation and visualization
library(lme4)          # For mixed effects models
library(lmerTest)      # For p-values in mixed models
library(see)           # For visualization of diagnostics
library(car)           # For additional diagnostics
library(DHARMa)        # For residual diagnostics of hierarchical models
library(gridExtra)     # For arranging multiple plots
library(cowplot)       # For plot layouts

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")

# Subset data to remove NA values - using tidyverse approach
data_slope <- data %>% filter(!is.na(Sleep_Slope_Matched))
data_amp <- data %>% filter(!is.na(Sleep_Amplitude))

# ------ FIT MODELS TO TEST ASSUMPTIONS ------

# Fit all models for sleep slope
slope_models <- list(
  Amplitude = lmer(Sleep_Slope_Matched ~ Age + Amplitude + (1|Participant), data = data_slope),
  Duration = lmer(Sleep_Slope_Matched ~ Age + Duration + (1|Participant), data = data_slope),
  Offset = lmer(Sleep_Slope_Matched ~ Age + Offset + (1|Participant), data = data_slope),
  Exponent = lmer(Sleep_Slope_Matched ~ Age + Exponent + (1|Participant), data = data_slope)
)

# Fit all models for sleep amplitude
amp_models <- list(
  Amplitude = lmer(Sleep_Amplitude ~ Age + Amplitude + (1|Participant), data = data_amp),
  Duration = lmer(Sleep_Amplitude ~ Age + Duration + (1|Participant), data = data_amp),
  Offset = lmer(Sleep_Amplitude ~ Age + Offset + (1|Participant), data = data_amp),
  Exponent = lmer(Sleep_Amplitude ~ Age + Exponent + (1|Participant), data = data_amp)
)

# ------ COMPREHENSIVE MODEL DIAGNOSTICS FUNCTION ------

comprehensive_diagnostics <- function(model, model_name, outcome_name) {
  cat("\n===== DIAGNOSTICS FOR", model_name, "PREDICTING", outcome_name, "=====\n")
  
  # Create a list to store all diagnostic plots
  plots <- list()
  
  # 1. Basic residual diagnostics
  cat("\n1. Basic Residual Diagnostics\n")
  
  # Extract residuals and fitted values
  res <- residuals(model)
  fitted <- fitted(model)
  
  # Residuals vs Fitted plot
  p1 <- ggplot(data.frame(fitted = fitted, residuals = res), aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = paste("Residuals vs Fitted -", model_name),
         x = "Fitted values",
         y = "Residuals") +
    theme_bw()
  plots$res_fitted <- p1
  
  # QQ plot for residuals
  p2 <- ggplot(data.frame(residuals = res), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("Normal Q-Q Plot -", model_name),
         x = "Theoretical quantiles",
         y = "Sample quantiles") +
    theme_bw()
  plots$qq <- p2
  
  # Histogram of residuals
  p3 <- ggplot(data.frame(residuals = res), aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    geom_density(alpha = 0.2, fill = "blue") +
    labs(title = paste("Histogram of Residuals -", model_name),
         x = "Residuals",
         y = "Count") +
    theme_bw()
  plots$hist <- p3
  
  # 2. Check for normality with Shapiro-Wilk test
  cat("\n2. Normality Test\n")
  sw_test <- shapiro.test(res)
  cat("Shapiro-Wilk normality test:\n")
  cat("W =", sw_test$statistic, ", p-value =", sw_test$p.value, "\n")
  if (sw_test$p.value < 0.05) {
    cat("The residuals appear to deviate from normality (p < 0.05).\n")
  } else {
    cat("The residuals appear to be normally distributed (p >= 0.05).\n")
  }
  
  # 3. Use performance package for comprehensive checks
  cat("\n3. Performance Package Model Checks\n")
  
  # Check model assumptions
  check_model_results <- try(check_model(model), silent = TRUE)
  if (!inherits(check_model_results, "try-error")) {
    plots$check_model <- check_model_results
  } else {
    cat("Could not generate check_model plot. Using individual checks instead.\n")
    
    # Check for normality of residuals
    norm_test <- check_normality(model)
    cat("Normality check: ", norm_test$message, "\n")
    
    # Check for heteroscedasticity
    homo_test <- check_heteroscedasticity(model)
    cat("Homoscedasticity check: ", homo_test$message, "\n")
  }
  
  # 4. Use DHARMa for mixed model specific diagnostics
  cat("\n4. DHARMa Residual Diagnostics\n")
  
  # Create DHARMa residuals
  sim_res <- simulateResiduals(model)
  
  # Plot DHARMa residuals
  dharma_res_plot <- plot(sim_res)
  plots$dharma <- dharma_res_plot
  
  # DHARMa tests
  cat("DHARMa tests for residual diagnostics:\n")
  testDispersion(sim_res)
  testZeroInflation(sim_res)
  testOutliers(sim_res)
  
  # 5. Check for influential observations
  cat("\n5. Influential Observations Check\n")
  
  # Using influence.ME or standard methods
  infl <- influence(model, obs = TRUE)
  cooks_d <- cooks.distance(infl)
  
  # Plot Cook's distances
  p4 <- ggplot(data.frame(index = 1:length(cooks_d), cooks_d = cooks_d), 
               aes(x = index, y = cooks_d)) +
    geom_point() +
    geom_hline(yintercept = 4/length(cooks_d), linetype = "dashed", color = "red") +
    labs(title = paste("Cook's Distance -", model_name),
         x = "Observation index",
         y = "Cook's distance") +
    theme_bw()
  plots$cooks <- p4
  
  # Check outliers
  high_influence <- which(cooks_d > (4/length(cooks_d)))
  if (length(high_influence) > 0) {
    cat("Potential influential observations found at indices:", 
        paste(high_influence, collapse = ", "), "\n")
  } else {
    cat("No highly influential observations detected.\n")
  }
  
  # 6. Check for multicollinearity
  cat("\n6. Check for Multicollinearity\n")
  
  # Get VIF values
  vif_values <- try(vif(model), silent = TRUE)
  if (!inherits(vif_values, "try-error")) {
    cat("Variance Inflation Factors:\n")
    print(vif_values)
    
    if (any(vif_values > 5)) {
      cat("Warning: Some VIF values are > 5, suggesting potential multicollinearity.\n")
    } else {
      cat("VIF values look acceptable (all < 5).\n")
    }
  } else {
    cat("Could not calculate VIF values for this model.\n")
    
    # Alternative: correlation between predictors
    model_data <- model.frame(model)
    # Remove response and random effects
    pred_data <- model_data[, !colnames(model_data) %in% c("(Intercept)", "(1|Participant)")]
    if (ncol(pred_data) > 1) {
      cor_matrix <- cor(pred_data, use = "complete.obs")
      cat("Correlation matrix between predictors:\n")
      print(cor_matrix)
      
      # Flag high correlations
      high_cors <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
      if (nrow(high_cors) > 0) {
        cat("Warning: High correlations (|r| > 0.7) found between predictors.\n")
      } else {
        cat("No high correlations found between predictors.\n")
      }
    }
  }
  
  # 7. Random effects assumptions
  cat("\n7. Random Effects Diagnostics\n")
  
  # Extract random effects
  re <- ranef(model)$Participant[,1]
  
  # QQ plot for random effects
  p5 <- ggplot(data.frame(re = re), aes(sample = re)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("Random Effects Q-Q Plot -", model_name),
         x = "Theoretical quantiles",
         y = "Sample quantiles") +
    theme_bw()
  plots$re_qq <- p5
  
  # Random effects histogram
  p6 <- ggplot(data.frame(re = re), aes(x = re)) +
    geom_histogram(bins = 20, fill = "lightgreen", color = "black") +
    geom_density(alpha = 0.2, fill = "green") +
    labs(title = paste("Random Effects Distribution -", model_name),
         x = "Random effects",
         y = "Count") +
    theme_bw()
  plots$re_hist <- p6
  
  # 8. Arrange plots
  p_combined <- plot_grid(
    p1, p2, p3, p4, p5, p6,
    ncol = 2, 
    labels = c("A", "B", "C", "D", "E", "F")
  )
  
  # Save combined plot
  plot_filename <- paste0("diagnostics_", gsub(" ", "_", tolower(model_name)), "_", 
                          gsub(" ", "_", tolower(outcome_name)), ".png")
  save_plot(plot_filename, p_combined, base_width = 10, base_height = 12)
  
  cat("\nDiagnostic plots saved to", plot_filename, "\n")
  
  # 9. Summary of findings
  cat("\n8. Summary of Assumption Checks\n")
  
  # Check normality
  norm_ok <- sw_test$p.value >= 0.05
  
  # Dispersion check from DHARMa
  disp_test <- testDispersion(sim_res, plot = FALSE)
  disp_ok <- disp_test$p.value >= 0.05
  
  # Create summary table
  assumption_summary <- data.frame(
    Assumption = c("Normality of Residuals", 
                   "Homogeneity of Variance", 
                   "Influential Observations",
                   "Multicollinearity"),
    Status = c(
      ifelse(norm_ok, "✓ Met", "✗ Violated"),
      ifelse(disp_ok, "✓ Met", "✗ Violated"),
      ifelse(length(high_influence) == 0, "✓ No issues", "⚠ Potential issues"),
      ifelse(!inherits(vif_values, "try-error") && all(vif_values < 5), 
             "✓ No issues", "⚠ Potential issues")
    ),
    Notes = c(
      ifelse(norm_ok, "Shapiro-Wilk p >= 0.05", paste("Shapiro-Wilk p =", round(sw_test$p.value, 4))),
      ifelse(disp_ok, "DHARMa dispersion test p >= 0.05", paste("DHARMa p =", round(disp_test$p.value, 4))),
      ifelse(length(high_influence) == 0, "No influential points detected",
             paste(length(high_influence), "potential influential points")),
      ifelse(!inherits(vif_values, "try-error"), 
             ifelse(all(vif_values < 5), "All VIF values < 5", "Some VIF values > 5"),
             "Could not calculate VIF")
    )
  )
  
  # Print summary table
  print(knitr::kable(assumption_summary))
  
  return(list(
    plots = plots,
    assumption_summary = assumption_summary
  ))
}

# ------ RUN DIAGNOSTICS FOR ALL MODELS ------

# Run diagnostics for the best slope model based on AIC
# Assuming "Amplitude" is the best predictor from previous analysis - change if needed
slope_diagnostics <- comprehensive_diagnostics(
  slope_models$Amplitude, 
  "Amplitude Model", 
  "Sleep Slope"
)

# Run diagnostics for the best amplitude model based on AIC
# Assuming "Amplitude" is the best predictor from previous analysis - change if needed
amp_diagnostics <- comprehensive_diagnostics(
  amp_models$Amplitude, 
  "Amplitude Model", 
  "Sleep Amplitude"
)

# ------ SAVE RESULTS ------

# Save diagnostics results
diagnostics_results <- list(
  slope_diagnostics = slope_diagnostics,
  amp_diagnostics = amp_diagnostics
)

# Save as RData
save(diagnostics_results, file = "wake_sleep_model_diagnostics.RData")

cat("\nModel assumptions testing complete. Results saved to wake_sleep_model_diagnostics.RData\n")