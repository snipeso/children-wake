function PeakFreq = find_periodic_peak(Data, Freqs, Range)
% recommendation: use smoothed data

[~, PeakFreqs, ~, Prominence] = findpeaks(log(Data), Freqs);

% only look at peaks within range
Keep = PeakFreqs >= Range(1) & PeakFreqs <= Range(2);
Prominence(~Keep) = [];
PeakFreqs(~Keep) = [];

% take the largest peak in range
[~, MaxProminance] = max(Prominence);
PeakFreq = PeakFreqs(MaxProminance);