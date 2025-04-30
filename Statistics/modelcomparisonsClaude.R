# After running the main analysis and having models 1-4 available

# 1. Compare models using AIC/BIC
AIC_values <- c(AIC(model1$finalModel), AIC(model2$finalModel), 
                AIC(model3$finalModel), AIC(model4$finalModel))
BIC_values <- c(BIC(model1$finalModel), BIC(model2$finalModel), 
                BIC(model3$finalModel), BIC(model4$finalModel))

model_names <- c("Age+Amplitude", "Age+Duration", "Age+Offset", "Age+Exponent")
names(AIC_values) <- model_names
names(BIC_values) <- model_names

cat("AIC values (lower is better):\n")
print(AIC_values)
cat("\nBest model by AIC:", names(which.min(AIC_values)), "\n\n")

cat("BIC values (lower is better):\n")
print(BIC_values)
cat("\nBest model by BIC:", names(which.min(BIC_values)), "\n\n")

# 2. Statistical comparison of cross-validation results
# Extract R-squared values for all models across folds
cv_values <- data.frame(
  Model1 = resamps$values[, "Age + Amplitude~Rsquared"],
  Model2 = resamps$values[, "Age + Duration~Rsquared"],
  Model3 = resamps$values[, "Age + Offset~Rsquared"],
  Model4 = resamps$values[, "Age + Exponent~Rsquared"]
)
colnames(cv_values) <- model_names

# Summarize CV values
cat("Cross-validation R-squared values summary:\n")
print(summary(cv_values))

# Plot the distributions
boxplot(cv_values, 
        main="Distribution of R-squared Values",
        xlab="Model", 
        ylab="R-squared")

# Perform paired t-tests between the best model and others
best_model_idx <- which.max(colMeans(cv_values))
best_model_name <- model_names[best_model_idx]

cat("\nPaired t-tests comparing best model (", best_model_name, ") with other models:\n")
for (i in 1:length(model_names)) {
  if (i != best_model_idx) {
    test_result <- t.test(cv_values[, best_model_idx], cv_values[, i], paired=TRUE)
    cat("vs", model_names[i], "p-value:", round(test_result$p.value, 4), 
        "95% CI:", round(test_result$conf.int[1], 4), "to", round(test_result$conf.int[2], 4), "\n")
    
    # Interpret results
    if (test_result$p.value < 0.05) {
      if (test_result$conf.int[1] > 0) {
        cat("  ", best_model_name, "is significantly better\n")
      } else {
        cat("  ", model_names[i], "is significantly better\n")
      }
    } else {
      cat("  No significant difference\n")
    }
  }
}

# 3. Non-parametric test (Friedman test)
cat("\nFriedman test for differences across all models:\n")
friedman_test <- friedman.test(as.matrix(cv_values))
print(friedman_test)

# If Friedman test is significant, perform post-hoc tests
if (friedman_test$p.value < 0.05) {
  cat("\nPost-hoc tests for Friedman (if package available):\n")
  if (require(PMCMRplus)) {
    posthoc <- frdAllPairsConoverTest(as.matrix(cv_values))
    print(posthoc)
  } else {
    cat("Package PMCMRplus not available. Install it for post-hoc tests.\n")
  }
}

# 4. Effect sizes of differences
cat("\nEffect sizes (Cohen's d) between best model and others:\n")
for (i in 1:length(model_names)) {
  if (i != best_model_idx) {
    # Calculate Cohen's d
    diff <- cv_values[, best_model_idx] - cv_values[, i]
    d <- mean(diff) / sd(diff)
    
    cat(best_model_name, "vs", model_names[i], "Cohen's d =", round(d, 2))
    
    # Interpret effect size
    if (abs(d) < 0.2) {
      cat(" (negligible effect)\n")
    } else if (abs(d) < 0.5) {
      cat(" (small effect)\n")
    } else if (abs(d) < 0.8) {
      cat(" (medium effect)\n")
    } else {
      cat(" (large effect)\n")
    }
  }
}

# 5. Bayesian model comparison (if BayesFactor package available)
cat("\nBayesian model comparison (if package available):\n")
if(require(BayesFactor)) {
  # Get length of data
  n <- nrow(data)
  
  # Create Bayesian t-tests
  for (i in 1:length(model_names)) {
    if (i != best_model_idx) {
      bf <- ttestBF(x = cv_values[, best_model_idx], y = cv_values[, i], paired = TRUE)
      cat(best_model_name, "vs", model_names[i], "Bayes Factor =", extractBF(bf)$bf, "\n")
      
      # Interpret Bayes Factor
      bf_value <- extractBF(bf)$bf
      if (bf_value > 100) {
        cat("  Extreme evidence for difference\n")
      } else if (bf_value > 30) {
        cat("  Very strong evidence for difference\n")
      } else if (bf_value > 10) {
        cat("  Strong evidence for difference\n")
      } else if (bf_value > 3) {
        cat("  Moderate evidence for difference\n")
      } else if (bf_value > 1) {
        cat("  Anecdotal evidence for difference\n")
      } else if (bf_value > 1/3) {
        cat("  Anecdotal evidence for no difference\n")
      } else if (bf_value > 1/10) {
        cat("  Moderate evidence for no difference\n")
      } else if (bf_value > 1/30) {
        cat("  Strong evidence for no difference\n")
      } else if (bf_value > 1/100) {
        cat("  Very strong evidence for no difference\n")
      } else {
        cat("  Extreme evidence for no difference\n")
      }
    }
  }
} else {
  cat("Package BayesFactor not available. Install it for Bayesian analysis.\n")
}

# 6. Overall conclusion
cat("\nOverall conclusion:\n")
# Check AIC/BIC best
aic_best <- which.min(AIC_values)
bic_best <- which.min(BIC_values)
cv_best <- which.max(colMeans(cv_values))

cat("Best model by AIC:", model_names[aic_best], "\n")
cat("Best model by BIC:", model_names[bic_best], "\n")  
cat("Best model by CV R-squared:", model_names[cv_best], "\n")

# Check if they all agree
if (aic_best == bic_best && bic_best == cv_best) {
  cat("\nAll criteria agree on the best model:", model_names[cv_best], "\n")
} else {
  cat("\nDifferent criteria suggest different best models.\n")
  cat("Consider other factors like interpretability, domain knowledge, or out-of-sample performance.\n")
}

# Final recommendation
cat("\nRecommended approach: If models are not significantly different, prefer the simpler one.\n")
cat("If they are significantly different, choose the one with the best performance.\n")