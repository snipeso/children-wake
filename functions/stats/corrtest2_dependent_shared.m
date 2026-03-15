function Stats = corrtest2_dependent_shared(r_jk, r_jh, r_kh, n)
% Compare two dependent correlations with one variable in common.
%
% This mirrors the QuantPsy "corrtest2" calculator:
% https://quantpsy.org/corrtest/corrtest2.htm
%
% Inputs
%   r_jk : first correlation
%   r_jh : second correlation sharing variable j
%   r_kh : correlation between the two unshared variables
%   n    : sample size
%
% Output
%   Stats : struct with the calculator-style intermediate values and p-values
%
% Method
%   Fisher r-to-z transform plus the dependent-overlapping correlation test
%   as implemented on the QuantPsy page by Lee & Preacher (2013).
% from codex, output checked by Sophia Snipes

validateattributes(r_jk, {'numeric'}, {'scalar', 'real', 'finite'})
validateattributes(r_jh, {'numeric'}, {'scalar', 'real', 'finite'})
validateattributes(r_kh, {'numeric'}, {'scalar', 'real', 'finite'})
validateattributes(n, {'numeric'}, {'scalar', 'real', 'finite', '>', 3})

if any(abs([r_jk, r_jh, r_kh]) >= 1)
    error('corrtest2_dependent_shared:InvalidCorrelation', ...
        'All correlations must be strictly between -1 and 1.');
end

fz_jk = fisher_r_to_z(r_jk);
fz_jh = fisher_r_to_z(r_jh);
fz_kh = fisher_r_to_z(r_kh);

diff_fz = fz_jk - fz_jh;
r_avg = (r_jk + r_jh) / 2;

cov_num_1 = r_kh * (1 - r_jk^2 - r_jh^2);
cov_num_2 = 0.5 * r_jk * r_jh * (1 - r_jk^2 - r_jh^2 - r_kh^2);
cov_denom = (1 - r_jk^2) * (1 - r_jh^2);
asymptotic_covariance = (cov_num_1 - cov_num_2) / cov_denom;

denominator = sqrt(2 - 2 * asymptotic_covariance);
z_score = sqrt(n - 3) * diff_fz / denominator;

one_tail_p = 1 - normal_cdf(abs(z_score));
two_tail_p = 2 * one_tail_p;

Stats = struct();
Stats.r_jk = r_jk;
Stats.r_jh = r_jh;
Stats.r_kh = r_kh;
Stats.n = n;
Stats.fisher_z_jk = fz_jk;
Stats.fisher_z_jh = fz_jh;
Stats.fisher_z_kh = fz_kh;
Stats.difference_fisher_z = diff_fz;
Stats.r_average = r_avg;
Stats.covariance_numerator_1 = cov_num_1;
Stats.covariance_numerator_2 = cov_num_2;
Stats.covariance_denominator = cov_denom;
Stats.asymptotic_covariance = asymptotic_covariance;
Stats.z_score = z_score;
Stats.one_tail_p = one_tail_p;
Stats.two_tail_p = two_tail_p;
Stats.status = 'OK';
end


function z = fisher_r_to_z(r)
z = 0.5 * log((1 + r) / (1 - r));
end


function p = normal_cdf(x)
p = 0.5 * erfc(-x / sqrt(2));
end
