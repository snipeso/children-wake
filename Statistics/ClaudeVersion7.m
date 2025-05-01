% MATLAB implementation of Wake-Sleep Analysis
% Converted from R script Version7_margand cond.R
% Relies on Statistics and Machine Learning Toolbox

% Clear workspace and command window
clear;
clc;

% Read data
% Note: Update the path to where your CSV file is located
data = readtable('D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv');

% Subset data to remove NA values
data_slope = data(~ismissing(data.Sleep_Slope_Matched), :);
data_amp = data(~ismissing(data.Sleep_Amplitude), :);

% Perform cross-validation
fprintf('\nPerforming cross-validation for Sleep Slope...\n');
cv_slope_results = cross_validate_mixed_models(data_slope, 'Sleep_Slope_Matched', 10);

fprintf('\nPerforming cross-validation for Sleep Amplitude...\n');
cv_amp_results = cross_validate_mixed_models(data_amp, 'Sleep_Amplitude', 10);

% Plot results
plot_slope = plot_cv_results(cv_slope_results, 'Sleep Slope');
plot_amp = plot_cv_results(cv_amp_results, 'Sleep Amplitude');

% Summarize results
slope_summary = summarize_cv_results(cv_slope_results, 'SLEEP SLOPE');
amp_summary = summarize_cv_results(cv_amp_results, 'SLEEP AMPLITUDE');

% Fit and compare models
[slope_comparison, slope_models] = fit_and_compare_models(data_slope, 'Sleep_Slope_Matched', 'SLEEP SLOPE');
[amp_comparison, amp_models] = fit_and_compare_models(data_amp, 'Sleep_Amplitude', 'SLEEP AMPLITUDE');

% ------ PRINT CONCLUSION ------

fprintf('\n===== FINAL CONCLUSION =====\n');
fprintf('The best wake predictor for Sleep Slope based on:\n');
fprintf('- Cross-validated Conditional R²: %s\n', slope_summary.Model(1));
fprintf('- AIC: %s\n', slope_comparison.Model(1));

% Sort by BIC
slope_bic_sorted = sortrows(slope_comparison, 'BIC');
fprintf('- BIC: %s\n\n', slope_bic_sorted.Model(1));

fprintf('The best wake predictor for Sleep Amplitude based on:\n');
fprintf('- Cross-validated Conditional R²: %s\n', amp_summary.Model(1));
fprintf('- AIC: %s\n', amp_comparison.Model(1));

% Sort by BIC
amp_bic_sorted = sortrows(amp_comparison, 'BIC');
fprintf('- BIC: %s\n', amp_bic_sorted.Model(1));

% ------ SAVE RESULTS ------

% Save figures
saveas(plot_slope.marginal, 'sleep_slope_marginal_r2.fig');
saveas(plot_slope.conditional, 'sleep_slope_conditional_r2.fig');
saveas(plot_amp.marginal, 'sleep_amplitude_marginal_r2.fig');
saveas(plot_amp.conditional, 'sleep_amplitude_conditional_r2.fig');

% Also save as PNG
saveas(plot_slope.marginal, 'sleep_slope_marginal_r2.png');
saveas(plot_slope.conditional, 'sleep_slope_conditional_r2.png');
saveas(plot_amp.marginal, 'sleep_amplitude_marginal_r2.png');
saveas(plot_amp.conditional, 'sleep_amplitude_conditional_r2.png');

% Save all results to MAT file
save('wake_sleep_analysis_results.mat', 'cv_slope_results', 'cv_amp_results', ...
    'slope_summary', 'amp_summary', 'slope_comparison', 'amp_comparison', ...
    'slope_models', 'amp_models');

fprintf('\nAnalysis complete. Results saved to wake_sleep_analysis_results.mat\n');

%% ===== FUNCTION DEFINITIONS =====

function results = cross_validate_mixed_models(data, outcome_var, k)
    % Create a results table
    results = table('Size', [0, 6], ...
        'VariableNames', {'Fold', 'Model', 'Marginal_R2', 'Conditional_R2', 'RMSE', 'MAE'}, ...
        'VariableTypes', {'double', 'string', 'double', 'double', 'double', 'double'});
    
    % Set random seed for reproducibility
    rng(123);
    
    % Create folds - stratified by participant if possible
    if ismember('Participant', data.Properties.VariableNames)
        % Get unique participants
        participants = unique(data.Participant);
        n_participants = length(participants);
        
        % Randomly assign participants to folds
        fold_assignments = mod(randperm(n_participants), k) + 1;
        
        % Create a lookup table instead of containers.Map
        participant_fold_table = table(participants, fold_assignments, 'VariableNames', {'Participant', 'Fold'});
        
        % Assign each row to a fold based on participant
        row_folds = zeros(height(data), 1);
        for i = 1:height(data)
            current_participant = data.Participant(i);
            participant_idx = find(participants == current_participant);
            row_folds(i) = fold_assignments(participant_idx);
        end
    else
        % Simple random assignment of rows to folds
        row_folds = mod(randperm(height(data)), k) + 1;
    end
    
    % Variables to model
    predictors = {'Amplitude', 'Duration', 'Offset', 'Exponent'};
    
    % Loop through folds
    for i = 1:k
        % Get training and test data
        train_idx = row_folds ~= i;
        test_idx = row_folds == i;
        train = data(train_idx, :);
        test = data(test_idx, :);
        
        % Loop through predictors to create and evaluate models
        for p = 1:length(predictors)
            pred = predictors{p};
            
            % Create formula for fitlme
            formula = [outcome_var, ' ~ Age + ', pred, ' + (1|Participant)'];
            
            % Fit model on training data
            model = fitlme(train, formula);
            
            % Get predictions on test data
            predictions = predict(model, test);
            
            % Calculate performance metrics
            actual = test.(outcome_var);
            rmse = sqrt(mean((actual - predictions).^2));
            mae = mean(abs(actual - predictions));
            
            % Calculate R-squared values
            % For fixed effects (marginal)
            fixed_formula = [outcome_var, ' ~ Age + ', pred];
            fixed_model = fitlm(train, fixed_formula);
            marginal_r2 = fixed_model.Rsquared.Ordinary;
            
            % For full model (conditional) - approximation
            % In MATLAB, we'll use a simplified approach to get conditional R²
            % True conditional R² includes both fixed and random effects
            conditional_r2 = model.Rsquared.Adjusted;
            
            % Add to results
            new_row = {double(i), string(pred), marginal_r2, conditional_r2, rmse, mae};
            results = [results; cell2table(new_row, 'VariableNames', results.Properties.VariableNames)];
        end
    end
end

function plots = plot_cv_results(cv_results, title_text)
    % Get unique models
    models = unique(cv_results.Model);
    
    % Create figure for Marginal R²
    figure('Name', [title_text, ' - Marginal R²']);
    marginal_data = zeros(length(models), 10); % Assuming 10-fold CV
    
    for i = 1:length(models)
        model_data = cv_results.Marginal_R2(strcmp(cv_results.Model, models{i}));
        marginal_data(i, 1:length(model_data)) = model_data';
    end
    
    boxplot(marginal_data', 'Labels', models);
    title([title_text, ' - Marginal R² Values']);
    ylabel('Marginal R²');
    xlabel('Predictor');
    subtitle('Fixed Effects Only (Age + Wake Measure)');
    
    % Create figure for Conditional R²
    figure('Name', [title_text, ' - Conditional R²']);
    conditional_data = zeros(length(models), 10); % Assuming 10-fold CV
    
    for i = 1:length(models)
        model_data = cv_results.Conditional_R2(strcmp(cv_results.Model, models{i}));
        conditional_data(i, 1:length(model_data)) = model_data';
    end
    
    boxplot(conditional_data', 'Labels', models);
    title([title_text, ' - Conditional R² Values']);
    ylabel('Conditional R²');
    xlabel('Predictor');
    subtitle('Fixed + Random Effects (Including Participant)');
    
    % Return handles to both plots
    plots.marginal = gcf;
    plots.conditional = gcf;
end

function summary_table = summarize_cv_results(cv_results, title_text)
    fprintf('\n===== SUMMARY OF %s CROSS-VALIDATION RESULTS =====\n', title_text);
    
    % Get unique models
    models = unique(cv_results.Model);
    
    % Create summary table
    summary_table = table('Size', [length(models), 7], ...
        'VariableNames', {'Model', 'Mean_Marginal_R2', 'SD_Marginal_R2', ...
                         'Mean_Conditional_R2', 'SD_Conditional_R2', ...
                         'Mean_RMSE', 'Mean_MAE'}, ...
        'VariableTypes', {'string', 'double', 'double', 'double', 'double', 'double', 'double'});
    
    % Populate summary table
    for i = 1:length(models)
        % Get rows for this model
        model_rows = strcmp(cv_results.Model, models{i});
        
        % Calculate metrics
        summary_table.Model(i) = models{i};
        summary_table.Mean_Marginal_R2(i) = mean(cv_results.Marginal_R2(model_rows));
        summary_table.SD_Marginal_R2(i) = std(cv_results.Marginal_R2(model_rows));
        summary_table.Mean_Conditional_R2(i) = mean(cv_results.Conditional_R2(model_rows));
        summary_table.SD_Conditional_R2(i) = std(cv_results.Conditional_R2(model_rows));
        summary_table.Mean_RMSE(i) = mean(cv_results.RMSE(model_rows));
        summary_table.Mean_MAE(i) = mean(cv_results.MAE(model_rows));
    end
    
    % Sort by highest conditional R²
    summary_table = sortrows(summary_table, 'Mean_Conditional_R2', 'descend');
    
    % Display table
    disp(summary_table);
    
    % Find best predictor based on conditional R²
    best_predictor = summary_table.Model(1);
    fprintf('\nBest predictor (highest conditional R²): %s\n', best_predictor);
end

function [comparison_table, models] = fit_and_compare_models(data, outcome_var, title_text)
    fprintf('\n===== FINAL MODEL COMPARISON FOR %s =====\n', title_text);
    
    % Variables to model
    predictors = {'Amplitude', 'Duration', 'Offset', 'Exponent'};
    
    % Create containers for models and comparison table
    models = struct();
    comparison_table = table('Size', [length(predictors), 8], ...
        'VariableNames', {'Model', 'AIC', 'BIC', 'Marginal_R2', 'Conditional_R2', ...
                         'Fixed_Effect_Estimate', 'Fixed_Effect_SE', 'Fixed_Effect_p'}, ...
        'VariableTypes', {'string', 'double', 'double', 'double', 'double', ...
                         'double', 'double', 'double'});
    
    % Fit models
    for i = 1:length(predictors)
        pred = predictors{i};
        
        % Create formula
        formula = [outcome_var, ' ~ Age + ', pred, ' + (1|Participant)'];
        
        % Fit model
        models.(pred) = fitlme(data, formula);
        
        % Get AIC and BIC
        model_aic = models.(pred).ModelCriterion.AIC;
        model_bic = models.(pred).ModelCriterion.BIC;
        
        % Get R² values
        fixed_formula = [outcome_var, ' ~ Age + ', pred];
        fixed_model = fitlm(data, fixed_formula);
        marginal_r2 = fixed_model.Rsquared.Ordinary;
        conditional_r2 = models.(pred).Rsquared.Adjusted;  % Approximation
        
        % Get fixed effect estimates
        coefs = models.(pred).Coefficients;
        pred_idx = strcmp(coefs.Name, pred);
        estimate = coefs.Estimate(pred_idx);
        se = coefs.SE(pred_idx);
        p_value = coefs.pValue(pred_idx);
        
        % Add to table
        comparison_table.Model(i) = pred;
        comparison_table.AIC(i) = model_aic;
        comparison_table.BIC(i) = model_bic;
        comparison_table.Marginal_R2(i) = marginal_r2;
        comparison_table.Conditional_R2(i) = conditional_r2;
        comparison_table.Fixed_Effect_Estimate(i) = estimate;
        comparison_table.Fixed_Effect_SE(i) = se;
        comparison_table.Fixed_Effect_p(i) = p_value;
    end
    
    % Sort by AIC
    comparison_table = sortrows(comparison_table, 'AIC');
    
    % Display table
    disp(comparison_table);
    
    % Print best model
    fprintf('\nBest model according to AIC: %s\n', comparison_table.Model(1));
    
    % Sort by BIC and get best model
    bic_sorted = sortrows(comparison_table, 'BIC');
    fprintf('Best model according to BIC: %s\n', bic_sorted.Model(1));
    
    % Print summary of best model
    fprintf('\nSummary of best model:\n');
    disp(models.(comparison_table.Model(1)));
end