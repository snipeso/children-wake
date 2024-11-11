function VIF = gpt_vif(Model)

X = designMatrix(Model, 'Fixed');

numPredictors = size(X, 2);
VIF = zeros(numPredictors, 1);

for j = 1:numPredictors
    % Select all predictors except the j-th
    otherPredictors = X(:, setdiff(1:numPredictors, j));
    targetPredictor = X(:, j);

    % Fit a linear regression model to predict the j-th predictor
    lm = fitlm(otherPredictors, targetPredictor);

    % Get the R-squared value for this regression
    R2_j = lm.Rsquared.Ordinary;

    % Calculate VIF for the j-th predictor
    VIF(j) = 1 / (1 - R2_j);
end

