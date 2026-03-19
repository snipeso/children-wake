function [Exponent, Offset] = simple_fooof_fit(X, Y, Range)
% Minimal exponent/offset wrapper used during preprocessing.

Results = fooof(X, Y, Range, struct(), false);

Exponent = -Results.aperiodic_params(2);
Offset = Results.aperiodic_params(1);
