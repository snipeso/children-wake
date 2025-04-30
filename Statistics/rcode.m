%% Read data
% Read the CSV file
data = readtable('D:/Data/AllWake/Results/children-wake/poster/SleepWakeStatsStandaridzed/WakeSleepAllData.csv');

%% Process data - Initial model comparison
% Create initial models for comparison
f1 = fitlm(data, 'Sleep_Slope_Matched ~ Age + Amplitude');
f2 = fitlm(data, 'Sleep_Slope_Matched ~ Age + Duration');
f3 = fitlm(data, 'Sleep_Slope_Matched ~ Age + Exponent');
f4 = fitlm(data, 'Sleep_Slope_Matched ~ Age + Offset');

% Display adjusted R-squared values
disp('Adjusted R-squared values:');
disp(['Model 1 (Age + Amplitude): ', num2str(f1.Rsquared.Adjusted)]);
disp(['Model 2 (Age + Duration): ', num2str(f2.Rsquared.Adjusted)]);
disp(['Model 3 (Age + Exponent): ', num2str(f3.Rsquared.Adjusted)]);
disp(['Model 4 (Age + Offset): ', num2str(f4.Rsquared.Adjusted)]);

%% Remove missing values
% Remove rows with NaN in Sleep_Slope_Matched
data = data(~isnan(data.Sleep_Slope_Matched), :);

%% Cross-validation setup
% Set random seed for reproducibility
rng(123);

% Get unique participants
participants = unique(data.Participant);
numParticipants = length(participants);
numFolds = 10;

% Create participant-stratified folds using Statistics and Machine Learning Toolbox
cvIndices = cell(numFolds, 1);
c = cvpartition(numParticipants, 'KFold', numFolds);
participantIndices = zeros(numParticipants, 1);
for fold = 1:numFolds
    participantIndices(c.test(fold)) = fold;
end

% Set up containers for results
n = height(data);
rsquared1 = zeros(numFolds, 1);
rsquared2 = zeros(numFolds, 1);
rsquared3 = zeros(numFolds, 1);
rsquared4 = zeros(numFolds, 1);

%% Cross-validation - Model 1 (Age + Amplitude)
fprintf('Running cross-validation for Model 1: Sleep_Slope_Matched ~ Age + Amplitude\n');
for fold = 1:numFolds
    % Get participant indices for this fold
    testParticipants = participants(participantIndices == fold);
    
    % Create test indices based on participants
    testIndices = ismember(data.Participant, testParticipants);
    trainIndices = ~testIndices;
    
    % Train model on training data
    model = fitlm(data(trainIndices, :), 'Sleep_Slope_Matched ~ Age + Amplitude');
    
    % Predict on test data
    y_test = data.Sleep_Slope_Matched(testIndices);
    y_pred = predict(model, data(testIndices, :));
    
    % Calculate R-squared for this fold
    SST = sum((y_test - mean(y_test)).^2);
    SSE = sum((y_test - y_pred).^2);
    rsquared1(fold) = 1 - SSE/SST;
end

%% Cross-validation - Model 2 (Age + Duration)
fprintf('Running cross-validation for Model 2: Sleep_Slope_Matched ~ Age + Duration\n');
for fold = 1:numFolds
    % Get participant indices for this fold
    testParticipants = participants(participantIndices == fold);
    
    % Create test indices based on participants
    testIndices = ismember(data.Participant, testParticipants);
    trainIndices = ~testIndices;
    
    % Train model on training data
    model = fitlm(data(trainIndices, :), 'Sleep_Slope_Matched ~ Age + Duration');
    
    % Predict on test data
    y_test = data.Sleep_Slope_Matched(testIndices);
    y_pred = predict(model, data(testIndices, :));
    
    % Calculate R-squared for this fold
    SST = sum((y_test - mean(y_test)).^2);
    SSE = sum((y_test - y_pred).^2);
    rsquared2(fold) = 1 - SSE/SST;
end

%% Cross-validation - Model 3 (Age + Offset)
fprintf('Running cross-validation for Model 3: Sleep_Slope_Matched ~ Age + Offset\n');
for fold = 1:numFolds
    % Get participant indices for this fold
    testParticipants = participants(participantIndices == fold);
    
    % Create test indices based on participants
    testIndices = ismember(data.Participant, testParticipants);
    trainIndices = ~testIndices;
    
    % Train model on training data
    model = fitlm(data(trainIndices, :), 'Sleep_Slope_Matched ~ Age + Offset');
    
    % Predict on test data
    y_test = data.Sleep_Slope_Matched(testIndices);
    y_pred = predict(model, data(testIndices, :));
    
    % Calculate R-squared for this fold
    SST = sum((y_test - mean(y_test)).^2);
    SSE = sum((y_test - y_pred).^2);
    rsquared3(fold) = 1 - SSE/SST;
end

%% Cross-validation - Model 4 (Age + Exponent)
fprintf('Running cross-validation for Model 4: Sleep_Slope_Matched ~ Age + Exponent\n');
for fold = 1:numFolds
    % Get participant indices for this fold
    testParticipants = participants(participantIndices == fold);
    
    % Create test indices based on participants
    testIndices = ismember(data.Participant, testParticipants);
    trainIndices = ~testIndices;
    
    % Train model on training data
    model = fitlm(data(trainIndices, :), 'Sleep_Slope_Matched ~ Age + Exponent');
    
    % Predict on test data
    y_test = data.Sleep_Slope_Matched(testIndices);
    y_pred = predict(model, data(testIndices, :));
    
    % Calculate R-squared for this fold
    SST = sum((y_test - mean(y_test)).^2);
    SSE = sum((y_test - y_pred).^2);
    rsquared4(fold) = 1 - SSE/SST;
end

%% Visualize results
% Create boxplots to compare models
figure;
boxplot([rsquared1, rsquared2, rsquared3, rsquared4], ...
    'Labels', {'Age+Amplitude', 'Age+Duration', 'Age+Offset', 'Age+Exponent'}, ...
    'Notch', 'on');
title('Cross-validation R-squared Values by Model');
ylabel('R-squared');
grid on;

% Display mean R-squared values for each model
fprintf('\nMean R-squared values across folds:\n');
fprintf('Model 1 (Age + Amplitude): %.4f\n', mean(rsquared1));
fprintf('Model 2 (Age + Duration): %.4f\n', mean(rsquared2));
fprintf('Model 3 (Age + Offset): %.4f\n', mean(rsquared3));
fprintf('Model 4 (Age + Exponent): %.4f\n', mean(rsquared4));