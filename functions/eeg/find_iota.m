function [PeakFrequency, Amplitude, Status] = find_iota(Power, Freqs)
% Status: 0 no peak found, 1: peak found, with harmonic. 2: small peak found, no harmonic;
% 3: large peak found, no harmonic; 4: large peak found, no other periodic
% NB: if there's a peak alpha, but in it both sinusoidal and not signal,
% the peak alpha might be off, and iota is still a harmonic

% finds largest peak in range with minimum prominenence


% checks for other major peaks, if they have a larger prominance than iota,
% and if they are within a minimum range of Hz to be a harmonic