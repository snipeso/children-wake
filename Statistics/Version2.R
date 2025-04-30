library(tidyverse)
library(lme4)       # For mixed effects models
library(lmerTest)   # For p-values in mixed models
library(caret)      # For cross-validation
library(MuMIn)      # For AIC calculation with mixed models
library(boot)       # For bootstrap confidence intervals

# Read data
data <- read.csv("D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv")
data$ParticipantSession <- paste(data$Participant, data$Session, sep = "_")

# Function to analyze both sleep measures in a unified way
analyze_sleep_measure <- function(outcome_var) {
  # Filter data
  analysis_data <- subset(data, !is.na(data[[outcome_var]]))
  
  # Set up cross-validation
  set.seed(123)
  train_control <- trainControl(
    method = "repeatedcv",     
    number = 10,              
    repeats = 5,              
    index = createMultiFolds(analysis_data$ParticipantSession, k = 10, times = 5)
  )
  
  # Create model formulas
  wake_predictors <- c("Amplitude", "Duration", "Offset", "Exponent")
  formulas <- list()
  
  for (i in 1:length(wake_predictors)) {
    formula_str <- paste(outcome_var, "~ Age +", wake_predictors[i], "+ (1|Participant)")
    formulas[[i]] <- as.formula(formula_str)
  }
  
  # Function to evaluate models in cross-validation
  eval_model <- function(formula, data, train_indices) {
    tryCatch({
      train_data <- data[train_indices, ]
      test_data <- data[-train_indices, ]
      
      model <- lmer(formula, data = train_data, REML = FALSE)
      predictions <- predict(model, newdata = test_data, allow.new.levels = TRUE)
      
      # Calculate R² (1 - SSE/SST)
      outcome_values <- test_data[[outcome_var]]
      ss_total <- sum((outcome_values - mean(outcome_values))^2, na.rm = TRUE)
      ss_error <- sum((outcome_values - predictions)^2, na.rm = TRUE)
      r_squared <- 1 - ss_error/ss_total
      
      return(list(RMSE = sqrt(mean((outcome_values - predictions)^2, na.rm = TRUE)), 
                  Rsquared = r_squared))
    }, error = function(e) {
      return(list(RMSE = NA, Rsquared = NA))
    })
  }
  
  # Run cross-validation for all models
  cv_results <- list()
  for (fold in 1:length(train_control$index)) {
    fold_results <- lapply(formulas, function(formula) {
      eval_model(formula, analysis_data, train_control$index[[fold]])
    })
    cv_results[[fold]] <- fold_results
  }
  
  # Extract R-squared values
  cv_rsq <- data.frame(matrix(NA, nrow = length(cv_results), ncol = length(formulas)))
  for (i in 1:length(cv_results)) {
    for (j in 1:length(formulas)) {
      cv_rsq[i, j] <- cv_results[[i]][[j]]$Rsquared
    }
  }
  
  # Set column names
  model_names <- paste0("Age+", wake_predictors)
  colnames(cv_rsq) <- model_names
  
  # Remove NA rows if any
  cv_rsq <- na.omit(cv_rsq)
  
  # Fit full models for AIC/BIC and store them with consistent naming
  full_models <- list()
  for (i in 1:length(formulas)) {
    # Use the same name as in model_names for consistency
    model_name <- model_names[i]
    tryCatch({
      full_models[[model_name]] <- lmer(formulas[[i]], data = analysis_data, REML = FALSE)
    }, error = function(e) {
      cat("Error fitting model", model_name, "for", outcome_var, ":", e$message, "\n")
    })
  }
  
  # Calculate AIC/BIC with consistent naming
  if (length(full_models) > 0) {
    AIC_values <- sapply(full_models, AIC)
    BIC_values <- sapply(full_models, BIC)
    # Names are already correct because we used model_names when creating full_models
  } else {
    AIC_values <- BIC_values <- NULL
    cat("No valid models to compare for", outcome_var, "\n")
  }
  
  # Create plots
  pdf(paste0(gsub("_", "_", tolower(outcome_var)), "_rsquared.pdf"), width=8, height=6)
  boxplot(cv_rsq, 
          main=paste("Cross-Validation R² Values for", outcome_var, "Models"),
          xlab="Model", 
          ylab="R-squared",
          col=ifelse(grepl("Slope", outcome_var), "lightblue", "lightgreen"),
          border=ifelse(grepl("Slope", outcome_var), "darkblue", "darkgreen"))
  points(colMeans(cv_rsq), col="red", pch=19)
  text(1:ncol(cv_rsq), colMeans(cv_rsq) + 0.03, 
       labels=round(colMeans(cv_rsq), 3), col="red")
  dev.off()
  
  # Print results
  cat("============= ", outcome_var, " RESULTS =============\n")
  if (!is.null(AIC_values)) {
    cat("AIC values (lower is better):\n")
    print(AIC_values)
    cat("\nBest model by AIC:", names(which.min(AIC_values)), "\n\n")
    
    cat("BIC values (lower is better):\n")
    print(BIC_values)
    cat("\nBest model by BIC:", names(which.min(BIC_values)), "\n\n")
  }
  
  cat("Cross-validation R-squared values summary:\n")
  print(summary(cv_rsq))
  
  # Statistical comparisons
  best_model_idx <- which.max(colMeans(cv_rsq))
  best_model_name <- colnames(cv_rsq)[best_model_idx]
  
  cat("\nBest model by CV R-squared:", best_model_name, "\n")
  cat("CV R-squared values for all models:\n")
  cv_means <- colMeans(cv_rsq)
  names(cv_means) <- colnames(cv_rsq)
  print(round(cv_means, 4))
  
  cat("\nComparing best model with others:\n")
  
  for (i in 1:ncol(cv_rsq)) {
    if (i != best_model_idx) {
      # T-test
      test_result <- t.test(cv_rsq[, best_model_idx], cv_rsq[, i], paired=TRUE)
      significant <- test_result$p.value < 0.05
      direction <- ifelse(test_result$conf.int[1] > 0, "better", "worse")
      
      # Bootstrap CI
      r2_diff <- cv_rsq[, best_model_idx] - cv_rsq[, i]
      boot_diff <- function(data, indices) mean(data[indices])
      boot_results <- boot(r2_diff, boot_diff, R=1000)
      boot_ci <- boot.ci(boot_results, type="perc")
      
      cat(best_model_name, "vs", colnames(cv_rsq)[i], ":\n")
      cat("  T-test p-value:", round(test_result$p.value, 4), 
          ifelse(significant, paste("(significantly", direction, ")"), "(no significant difference)"), "\n")
      cat("  Bootstrap 95% CI:", round(boot_ci$percent[4], 4), "to", round(boot_ci$percent[5], 4), "\n\n")
    }
  }
  
  # Now compare AIC best model with CV best model if they differ
  aic_best_idx <- which.min(AIC_values)
  aic_best_name <- names(AIC_values)[aic_best_idx]
  
  if (aic_best_name != best_model_name) {
    cat("\nDiscrepancy between AIC and CV R-squared best models:\n")
    cat("AIC best model:", aic_best_name, "vs CV best model:", best_model_name, "\n")
    
    # Find the CV R-squared index that corresponds to the AIC best model
    aic_best_cv_idx <- which(colnames(cv_rsq) == aic_best_name)
    
    # T-test between CV best and AIC best
    test_result <- t.test(cv_rsq[, best_model_idx], cv_rsq[, aic_best_cv_idx], paired=TRUE)
    significant <- test_result$p.value < 0.05
    if (significant) {
      if (test_result$conf.int[1] > 0) {
        explanation <- paste("CV R-squared best model (", best_model_name, ") has significantly better predictive performance")
      } else {
        explanation <- paste("AIC best model (", aic_best_name, ") has significantly better predictive performance, 
                             despite slightly lower mean CV R-squared")
      }
    } else {
      explanation <- "The difference in predictive performance is not statistically significant"
    }
    
    cat("  T-test p-value:", round(test_result$p.value, 4), "\n")
    cat("  Explanation:", explanation, "\n\n")
    
    cat("  AIC difference:", AIC_values[names(AIC_values) == best_model_name] - AIC_values[aic_best_name], 
        "(positive means AIC best model has better fit)\n")
    cat("  CV R-squared difference:", mean(cv_rsq[, best_model_idx]) - mean(cv_rsq[, aic_best_cv_idx]), 
        "(positive means CV best model has better predictive performance)\n\n")
  }
  
  # Return results for further analysis
  return(list(
    cv_rsq = cv_rsq,
    AIC_values = AIC_values,
    BIC_values = BIC_values,
    full_models = full_models,
    best_model_idx = best_model_idx,
    best_model_name = best_model_name,
    aic_best_name = aic_best_name
  ))
}

# Analyze both outcome variables
slope_results <- analyze_sleep_measure("Sleep_Slope_Matched")
amplitude_results <- analyze_sleep_measure("Sleep_Amplitude_Matched")

# Overall conclusion
cat("\n\n============= OVERALL CONCLUSION =============\n")

# Function to print best models
print_best_models <- function(results, outcome_name) {
  cat("Best model for", outcome_name, ":\n")
  
  if (!is.null(results$AIC_values)) {
    cat("By AIC:", results$aic_best_name, "\n")
    cat("By BIC:", names(which.min(results$BIC_values)), "\n")
  }
  
  cat("By CV R-squared:", results$best_model_name, "\n\n")
  
  # Print detailed stats for AIC best model
  if (!is.null(results$AIC_values)) {
    best_model <- results$full_models[[results$aic_best_name]]
    cat("Detailed statistics for AIC best model (", results$aic_best_name, "):\n")
    print(summary(best_model))
    
    # Only print CV best model if different from AIC best
    if (results$aic_best_name != results$best_model_name) {
      cat("\nDetailed statistics for CV best model (", results$best_model_name, "):\n")
      print(summary(results$full_models[[results$best_model_name]]))
    }
  }
}

print_best_models(slope_results, "Sleep Slope")
print_best_models(amplitude_results, "Sleep Amplitude")

# Create a combined boxplot
pdf("combined_rsquared_boxplot.pdf", width=10, height=6)
par(mfrow=c(1,2))

# Slope boxplot
boxplot(slope_results$cv_rsq, 
        main="Sleep Slope Models",
        xlab="Model", 
        ylab="R-squared",
        col="lightblue",
        border="darkblue")
points(colMeans(slope_results$cv_rsq), col="red", pch=19)
text(1:ncol(slope_results$cv_rsq), colMeans(slope_results$cv_rsq) + 0.03, 
     labels=round(colMeans(slope_results$cv_rsq), 3), col="red")

# Amplitude boxplot
boxplot(amplitude_results$cv_rsq, 
        main="Sleep Amplitude Models",
        xlab="Model", 
        ylab="R-squared",
        col="lightgreen",
        border="darkgreen")
points(colMeans(amplitude_results$cv_rsq), col="red", pch=19)
text(1:ncol(amplitude_results$cv_rsq), colMeans(amplitude_results$cv_rsq) + 0.03, 
     labels=round(colMeans(amplitude_results$cv_rsq), 3), col="red")

dev.off()