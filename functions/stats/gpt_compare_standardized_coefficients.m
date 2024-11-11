function [p_value, z_statistic] = gpt_compare_standardized_coefficients(Model1, Model2, Factor1, Factor2)

% Extract standardized coefficients and standard errors
beta_Z_V1 = Model1.Coefficients.Estimate(strcmp(Model1.Coefficients.Name, Factor1));
beta_Z_V2 = Model2.Coefficients.Estimate(strcmp(Model2.Coefficients.Name, Factor2));

se_Z_V1 = Model1.Coefficients.SE(strcmp(Model1.Coefficients.Name, Factor1));
se_Z_V2 = Model2.Coefficients.SE(strcmp(Model2.Coefficients.Name, Factor2));

% Calculate the Z-statistic
z_statistic = (beta_Z_V1 - beta_Z_V2) / sqrt(se_Z_V1^2 + se_Z_V2^2);

% Calculate the p-value for the Z-statistic
p_value = 2 * (1 - normcdf(abs(z_statistic)));

% Display results
fprintf('Standardized Beta for %s: %.4f\n', Factor1, beta_Z_V1);
fprintf('Standardized Beta for %s: %.4f\n', Factor2, beta_Z_V2);
fprintf('Z-Statistic: %.4f\n', z_statistic);
fprintf('P-Value: %.4f\n', p_value);

% Interpretation
if p_value < 0.05
    fprintf('The difference between the standardized coefficients is statistically significant.\n');
else
    fprintf('The difference between the standardized coefficients is not statistically significant.\n');
end
