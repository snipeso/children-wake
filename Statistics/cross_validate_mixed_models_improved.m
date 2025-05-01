% Improved cross-validation function for mixed effects models in MATLAB
% This version better aligns with the R implementation

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
            
            try
                % Create formula for model
                formula = sprintf('%s ~ Age + %s + (1|Participant)', outcome_var, pred);
                
                % Fit model on training data
                lme = fitlme(train_data, formula);
                
                % Get predictions for test data (with handling for new participants)
                % MATLAB's predict method for fitlme handles new random effect levels differently than R
                % For new random effect levels, MATLAB uses a value of 0 for the random effect
                % This is somewhat similar to R's allow.new.levels=TRUE but not identical
                y_pred = predict(lme, test_data);
                y_true = test_data.(outcome_var);
                
                % Calculate standard metrics (RMSE, MAE)
                rmse = sqrt(mean((y_true - y_pred).^2));
                mae = mean(abs(y_true - y_pred));
                
                % Calculate pseudo R² (as in original code)
                ss_total = var(y_true) * length(y_true);
                ss_res = sum((y_true - y_pred).^2);
                pseudo_r2 = 1 - ss_res / ss_total;
                
                % Approximate marginal and conditional R² values
                % This is an approximation since MATLAB doesn't have r.squaredGLMM
                
                % For marginal R² (fixed effects only)
                % Get fixed effects design matrix
                X = [ones(height(train_data), 1), train_data.Age, train_data.(pred)];
                
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
                % This is a rough approximation - R's r.squaredGLMM is more sophisticated
                conditional_r2 = marginal_r2 + (var_total - var_fixed - var_residual) / var_total;
                
                % Ensure values are in valid range
                marginal_r2 = min(max(marginal_r2, 0), 1);
                conditional_r2 = min(max(conditional_r2, 0), 1);
                
                % Add results to table
                new_row = table(fold, string(pred), marginal_r2, conditional_r2, rmse, mae, pseudo_r2, ...
                    'VariableNames', {'Fold', 'Model', 'Marginal_R2', 'Conditional_R2', 'RMSE', 'MAE', 'Pseudo_R2'});
                cv_results = [cv_results; new_row];
                
            catch ME
                warning('Error in fold %d with predictor %s: %s', fold, pred, ME.message);
            end
        end
    end
end

% Original summary function that works with the existing codebase
function summary_table = summarize_cv_results(cv_results, title_str)
    fprintf('\n===== SUMMARY OF %s CROSS-VALIDATION RESULTS =====\n', upper(title_str));

    % Convert model column to categorical for grouping
    cv_results.Model = categorical(cv_results.Model);

    % Use `groupsummary` for compatibility with original code
    summary_table = groupsummary(cv_results, 'Model', ...
        {'mean', 'std'}, {'R2', 'RMSE', 'MAE'});

    % Rename columns
    summary_table.Properties.VariableNames = {'Model', ...
        'GroupCount', ...
        'Mean_R2', 'Std_R2', ...
        'Mean_RMSE', 'Std_RMSE', ...
        'Mean_MAE', 'Std_MAE'};

    % Sort by Mean_R2 descending
    summary_table = sortrows(summary_table, 'Mean_R2', 'descend');

    disp(summary_table);

    fprintf('\nBest predictor based on R²: %s\n', string(summary_table.Model(1)));
end

% Improved summary function to match R output (use this when using the improved cross-validation)
function summary_table = summarize_cv_results_improved(cv_results, title_str)
    fprintf('\n===== SUMMARY OF %s CROSS-VALIDATION RESULTS =====\n', upper(title_str));

    % Convert model column to categorical for grouping
    cv_results.Model = categorical(cv_results.Model);

    % Group by Model and calculate means and standard deviations
    models = unique(cv_results.Model);
    
    % Initialize summary table
    summary_table = table('Size', [length(models) 8], ...
        'VariableTypes', {'categorical', 'double', 'double', 'double', 'double', 'double', 'double', 'double'}, ...
        'VariableNames', {'Model', 'GroupCount', 'Mean_Marginal_R2', 'SD_Marginal_R2', ...
                           'Mean_Conditional_R2', 'SD_Conditional_R2', 'Mean_RMSE', 'Mean_MAE'});
    
    for i = 1:length(models)
        model = models(i);
        model_rows = cv_results.Model == model;
        
        summary_table.Model(i) = model;
        summary_table.GroupCount(i) = sum(model_rows);
        summary_table.Mean_Marginal_R2(i) = mean(cv_results.Marginal_R2(model_rows));
        summary_table.SD_Marginal_R2(i) = std(cv_results.Marginal_R2(model_rows));
        summary_table.Mean_Conditional_R2(i) = mean(cv_results.Conditional_R2(model_rows));
        summary_table.SD_Conditional_R2(i) = std(cv_results.Conditional_R2(model_rows));
        summary_table.Mean_RMSE(i) = mean(cv_results.RMSE(model_rows));
        summary_table.Mean_MAE(i) = mean(cv_results.MAE(model_rows));
    end
    
    % Sort by Mean_Conditional_R2 descending (to match R)
    summary_table = sortrows(summary_table, 'Mean_Conditional_R2', 'descend');

    disp(summary_table);

    fprintf('\nBest predictor based on Conditional R²: %s\n', string(summary_table.Model(1)));
end

% Improved plotting function to more closely match R ggplot output
function plot_cv_results_improved(cv_results, title_str)
    % Create subplot for Marginal R²
    figure('Position', [100, 100, 1200, 900]);
    
    subplot(2, 1, 1);
    boxplot(cv_results.Marginal_R2, cv_results.Model, 'Colors', [0.3 0.5 0.7]);
    title(sprintf('%s - Marginal R² Values (Fixed Effects Only)', title_str), 'FontSize', 14);
    ylabel('Marginal R²', 'FontSize', 12);
    xlabel('Predictor', 'FontSize', 12);
    grid on;
    
    % Create subplot for Conditional R²
    subplot(2, 1, 2);
    boxplot(cv_results.Conditional_R2, cv_results.Model, 'Colors', [0.3 0.7 0.5]);
    title(sprintf('%s - Conditional R² Values (Fixed + Random Effects)', title_str), 'FontSize', 14);
    ylabel('Conditional R²', 'FontSize', 12);
    xlabel('Predictor', 'FontSize', 12);
    grid on;
    
    % Add figure title
    sgtitle(sprintf('Cross-Validation Results for %s', title_str), 'FontSize', 16);
    
    % Optional: save figure
    saveas(gcf, sprintf('%s_r2_boxplots.png', lower(strrep(title_str, ' ', '_'))));
end


% Example usage:
% 
% % Load data
% data = readtable('WakeSleepAllData.csv');
% 
% % Remove rows with missing values for target outcomes
% data_slope = data(~ismissing(data.Sleep_Slope_Matched), :);
% data_amp = data(~ismissing(data.Sleep_Amplitude), :);
% 
% % Run improved cross-validation
% cv_slope_results = cross_validate_mixed_models_improved(data_slope, 'Sleep_Slope_Matched', 10);
% cv_amp_results = cross_validate_mixed_models_improved(data_amp, 'Sleep_Amplitude', 10);
% 
% % Summarize results
% slope_summary = summarize_cv_results_improved(cv_slope_results, 'Sleep Slope');
% amp_summary = summarize_cv_results_improved(cv_amp_results, 'Sleep Amplitude');
% 
% % Plot results
% plot_cv_results_improved(cv_slope_results, 'Sleep Slope');
% plot_cv_results_improved(cv_amp_results, 'Sleep Amplitude');