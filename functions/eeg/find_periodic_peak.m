function PeakFreq = find_periodic_peak(Data, Freqs, Range)
% Finds the strongest periodic peak within a requested frequency range.
% recommendation: use smoothed data

[~, PeakFreqs, ~, Prominence] = findpeaks(log10(Data), Freqs);

% only look at peaks within range
Keep = PeakFreqs >= Range(1) & PeakFreqs <= Range(2);
Prominence(~Keep) = [];
PeakFreqs(~Keep) = [];

% take the largest peak in range
[~, MaxProminance] = max(Prominence);
PeakFreq = PeakFreqs(MaxProminance);