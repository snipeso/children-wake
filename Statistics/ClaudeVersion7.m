clear
clc
close all

% Load data
data = readtable('D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv');

% Remove rows with missing values for target outcomes
data_slope = data(~ismissing(data.Sleep_Slope_Matched), :);
data_amp = data(~ismissing(data.Sleep_Amplitude), :);

% Cross-validation analysis
cv_slope_results = cross_validate_mixed_models_improved(data_slope, 'Sleep_Slope_Matched', 10);
cv_amp_results = cross_validate_mixed_models_improved(data_amp, 'Sleep_Amplitude', 10);

%%
% Summarize cross-validation results
slope_summary = summarize_cv_results(cv_slope_results, 'Sleep Slope');
amp_summary = summarize_cv_results(cv_amp_results, 'Sleep Amplitude');

% Plot cross-validation results
plot_cv_results_enhanced(cv_slope_results, 'Sleep Slope');
plot_cv_results_enhanced(cv_amp_results, 'Sleep Amplitude');

% Fit and compare final models
slope_models = fit_and_compare_models(data_slope, 'Sleep_Slope_Matched', 'SLEEP SLOPE');
amp_models = fit_and_compare_models(data_amp, 'Sleep_Amplitude', 'SLEEP AMPLITUDE');

% Print final conclusion
fprintf('\n===== FINAL CONCLUSION =====\n');
fprintf('The best wake predictor for Sleep Slope based on:\n');
fprintf('- Cross-validated Conditional R²: %s\n', slope_summary.Model(1));
fprintf('- AIC: %s\n', slope_models.comparison_table.Model(1));
fprintf('- BIC: %s\n', slope_models.comparison_table.Model{find(slope_models.comparison_table.BIC == min(slope_models.comparison_table.BIC))});

fprintf('\nThe best wake predictor for Sleep Amplitude based on:\n');
fprintf('- Cross-validated Conditional R²: %s\n', amp_summary.Model(1));
fprintf('- AIC: %s\n', amp_models.comparison_table.Model{1});
fprintf('- BIC: %s\n', amp_models.comparison_table.Model{find(amp_models.comparison_table.BIC == min(amp_models.comparison_table.BIC))});

% Save results
results = struct();
results.cv_slope_results = cv_slope_results;
results.cv_amp_results = cv_amp_results;
results.slope_summary = slope_summary;
results.amp_summary = amp_summary;
results.slope_models = slope_models;
results.amp_models = amp_models;

save('wake_sleep_analysis_results.mat', 'results');
fprintf('\nAnalysis complete. Results saved to wake_sleep_analysis_results.mat\n');

function cv_results = cross_validate_mixed_models_improved(data, outcome_var, k)
    % Ensure reproducibility - same seed as R
    rng(123);

    % List of predictors
    predictors = {'Amplitude', 'Duration', 'Offset', 'Exponent'};
    
    % Initialize structure for results
    cv_results = table('Size', [0 7], ...
        'VariableTypes', {'double', 'string', 'double', 'double', 'double', 'double', 'double'}, ...
        'VariableNames', {'Fold', 'Model', 'Marginal_R2', 'Conditional_R2', 'RMSE', 'MAE', 'Pseudo_R2'});
    
    % Ensure Participant is categorical
    if ~iscategorical(data.Participant)
        data.Participant = categorical(data.Participant);
    end
    
    % Get unique participants
    participants = unique(data.Participant);
    n_participants = length(participants);
    
    % Create R-like fold assignment (sample with replacement)
    % This mimics: participant_folds <- sample(1:k, length(participants), replace=TRUE)
    participant_folds = randi(k, n_participants, 1);
    
    % Process each fold
    for fold = 1:k
        % Find participants in this fold (test set)
        test_participant_indices = (participant_folds == fold);
        test_participants = participants(test_participant_indices);
        
        % Find participants not in this fold (training set)
        train_participants = participants(~test_participant_indices);
        
        % Skip if either set is empty or has insufficient participants
        if isempty(test_participants) || isempty(train_participants) || ...
           length(test_participants) < 2 || length(train_participants) < 2
            warning('Skipping fold %d due to insufficient participants', fold);
            continue;
        end
        
        % Create train and test datasets
        train_data = data(ismember(data.Participant, train_participants), :);
        test_data = data(ismember(data.Participant, test_participants), :);
        
        % Process each predictor
        for i = 1:length(predictors)
            pred = predictors{i};
            fprintf('Processing fold %d, predictor %s\n', fold, pred);
            
            % Create formula for model
            formula = sprintf('%s ~ Age + %s + (1|Participant)', outcome_var, pred);
            
            % Fit model on training data
            lme = fitlme(train_data, formula);
            
            % Get predictions for test data
            y_pred = predict(lme, test_data);
            y_true = test_data.(outcome_var);
            
            % Calculate standard metrics (RMSE, MAE)
            rmse = sqrt(mean((y_true - y_pred).^2));
            mae = mean(abs(y_true - y_pred));
            
            % Calculate pseudo R² (as in original code)
            ss_total = var(y_true) * length(y_true);
            ss_res = sum((y_true - y_pred).^2);
            pseudo_r2 = 1 - ss_res / ss_total;
            
            % Get better approximation of marginal and conditional R²
            % Instead of using covarianceParameters, use a simpler approach
            
            % For marginal R² (fixed effects only)
            % Get fixed effects design matrix
            X = [ones(height(train_data), 1), train_data.Age, train_data{:, pred}];
            
            % Get fixed effects coefficients
            beta = lme.fixedEffects;
            
            % Calculate fitted values (fixed effects only)
            fitted_fixed = X * beta;
            
            % Calculate variance of fitted values
            var_fixed = var(fitted_fixed);
            
            % Calculate residual variance
            residuals = train_data.(outcome_var) - fitted_fixed;
            var_residual = var(residuals);
            
            % Calculate total variance
            var_total = var(train_data.(outcome_var));
            
            % Approximate marginal R²
            marginal_r2 = var_fixed / var_total;
            
            % Approximate conditional R² (fixed + random effects)
            conditional_r2 = marginal_r2 + (var_total - var_fixed - var_residual) / var_total;
            
            % Ensure values are in valid range
            marginal_r2 = min(max(marginal_r2, 0), 1);
            conditional_r2 = min(max(conditional_r2, 0), 1);
            
            % Add results to table
            new_row = table(fold, string(pred), marginal_r2, conditional_r2, rmse, mae, pseudo_r2, ...
                'VariableNames', {'Fold', 'Model', 'Marginal_R2', 'Conditional_R2', 'RMSE', 'MAE', 'Pseudo_R2'});
            cv_results = [cv_results; new_row];
        end
    end
end

function plot_cv_results_enhanced(cv_results, title_str)
    % Create figure for marginal R²
    figure;
    
    % Convert to categorical for grouping if not already
    if ~iscategorical(cv_results.Model)
        cv_results.Model = categorical(cv_results.Model);
    end
    
    % Boxplot of Marginal R²
    boxplot(cv_results.Marginal_R2, cv_results.Model);
    title(sprintf('%s - Marginal R² (Fixed Effects Only)', title_str), 'Interpreter', 'none');
    ylabel('Marginal R²');
    xlabel('Predictor');
    grid on;
    
    % Save figure
    saveas(gcf, sprintf('%s_marginal_r2_boxplot.png', lower(strrep(title_str, ' ', '_'))));
    
    % Create figure for conditional R²
    figure;
    
    % Boxplot of Conditional R²
    boxplot(cv_results.Conditional_R2, cv_results.Model);
    title(sprintf('%s - Conditional R² (Fixed + Random Effects)', title_str), 'Interpreter', 'none');
    ylabel('Conditional R²');
    xlabel('Predictor');
    grid on;
    
    % Save figure
    saveas(gcf, sprintf('%s_conditional_r2_boxplot.png', lower(strrep(title_str, ' ', '_'))));
end

function summary_table = summarize_cv_results(cv_results, title_str)
    fprintf('\n===== SUMMARY OF %s CROSS-VALIDATION RESULTS =====\n', upper(title_str));

    % Convert model column to categorical for grouping if it's not already
    if ~iscategorical(cv_results.Model)
        cv_results.Model = categorical(cv_results.Model);
    end

    % Compute summary statistics grouped by Model
    models = unique(cv_results.Model);
    n_models = length(models);
    
    % Create summary table
    summary_table = table();
    summary_table.Model = models;
    summary_table.Mean_Marginal_R2 = zeros(n_models, 1);
    summary_table.SD_Marginal_R2 = zeros(n_models, 1);
    summary_table.Mean_Conditional_R2 = zeros(n_models, 1);
    summary_table.SD_Conditional_R2 = zeros(n_models, 1);
    summary_table.Mean_RMSE = zeros(n_models, 1);
    summary_table.Mean_MAE = zeros(n_models, 1);
    
    % Populate table
    for i = 1:n_models
        model_name = models(i);
        model_rows = cv_results.Model == model_name;
        
        summary_table.Mean_Marginal_R2(i) = mean(cv_results.Marginal_R2(model_rows));
        summary_table.SD_Marginal_R2(i) = std(cv_results.Marginal_R2(model_rows));
        summary_table.Mean_Conditional_R2(i) = mean(cv_results.Conditional_R2(model_rows));
        summary_table.SD_Conditional_R2(i) = std(cv_results.Conditional_R2(model_rows));
        summary_table.Mean_RMSE(i) = mean(cv_results.RMSE(model_rows));
        summary_table.Mean_MAE(i) = mean(cv_results.MAE(model_rows));
    end
    
    % Sort by Mean_Conditional_R2 descending
    summary_table = sortrows(summary_table, 'Mean_Conditional_R2', 'descend');

    % Display table with consistent formatting
    disp(summary_table);

    fprintf('\nBest predictor based on Conditional R²: %s\n', string(summary_table.Model(1)));
    
    return;
end

function result = fit_and_compare_models(data, outcome_var, title_str)
    fprintf('\n===== FINAL MODEL COMPARISON FOR %s =====\n', title_str);
    
    % Variables to model
    predictors = {'Amplitude', 'Duration', 'Offset', 'Exponent'};
    
    % Create table to store comparison results
    comparison_table = table('Size', [0 8], ...
        'VariableTypes', {'string', 'double', 'double', 'double', 'double', 'double', 'double', 'double'}, ...
        'VariableNames', {'Model', 'AIC', 'BIC', 'Marginal_R2', 'Conditional_R2', ...
                           'Fixed_Effect_Estimate', 'Fixed_Effect_SE', 'Fixed_Effect_p'});
    
    % Cell array to store models
    models = cell(length(predictors), 1);
    model_names = cell(length(predictors), 1);
    
    % Fit models for each predictor
    for i = 1:length(predictors)
        pred = predictors{i};
        fprintf('Fitting model with predictor: %s\n', pred);
        
        % Create formula for model
        formula = sprintf('%s ~ Age + %s + (1|Participant)', outcome_var, pred);
        
        % Fit model
        lme = fitlme(data, formula);
        
        % Store model and name
        models{i} = lme;
        model_names{i} = pred;
        
        % Get AIC and BIC
        aic_value = lme.ModelCriterion.AIC;
        bic_value = lme.ModelCriterion.BIC;
        
        % Calculate R² values using simpler approach
        % For marginal R² (fixed effects only)
        X = [ones(height(data), 1), data.Age, data{:, pred}];
        beta = lme.fixedEffects;
        fitted_fixed = X * beta;
        var_fixed = var(fitted_fixed);
        residuals = data.(outcome_var) - fitted_fixed;
        var_residual = var(residuals);
        var_total = var(data.(outcome_var));
        
        % Approximate marginal and conditional R²
        marginal_r2 = var_fixed / var_total;
        conditional_r2 = marginal_r2 + (var_total - var_fixed - var_residual) / var_total;
        
        % Ensure values are in valid range
        marginal_r2 = min(max(marginal_r2, 0), 1);
        conditional_r2 = min(max(conditional_r2, 0), 1);
        
        % Get fixed effect estimates
        coefs = lme.Coefficients;
        pred_row = find(strcmp(coefs.Name, pred));
        if ~isempty(pred_row)
            estimate = coefs.Estimate(pred_row);
            se = coefs.SE(pred_row);
            p_value = coefs.pValue(pred_row);
        else
            % Fallback in case the predictor name doesn't match exactly
            estimate = NaN;
            se = NaN;
            p_value = NaN;
            warning('Could not find coefficient for predictor %s', pred);
        end
        
        % Add to comparison table
        new_row = table(string(pred), aic_value, bic_value, marginal_r2, conditional_r2, ...
                        estimate, se, p_value, ...
                        'VariableNames', {'Model', 'AIC', 'BIC', 'Marginal_R2', 'Conditional_R2', ...
                                          'Fixed_Effect_Estimate', 'Fixed_Effect_SE', 'Fixed_Effect_p'});
        comparison_table = [comparison_table; new_row];
    end
    
    % Sort by AIC
    comparison_table = sortrows(comparison_table, 'AIC');
    
    % Display table
    disp(comparison_table);
    
    % Get best models
    best_aic_idx = 1; % First row after sorting by AIC
    [~, best_bic_idx] = min(comparison_table.BIC);
    
    best_aic_model = char(comparison_table.Model(best_aic_idx));
    best_bic_model = char(comparison_table.Model(best_bic_idx));
    
    fprintf('\nBest model according to AIC: %s\n', best_aic_model);
    fprintf('Best model according to BIC: %s\n', best_bic_model);
    
    % Find the index of the best AIC model in our models cell array
    best_model_idx = find(strcmp(model_names, best_aic_model));
    
    % Print summary of best model
    fprintf('\nSummary of best model:\n');
    if ~isempty(best_model_idx)
        disp(models{best_model_idx});
    else
        fprintf('Could not find best model in stored models.\n');
    end
    
    % Return results
    result = struct();
    result.comparison_table = comparison_table;
    result.models = models;
    result.model_names = model_names;
end