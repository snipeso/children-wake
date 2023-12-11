function EEG = center_eeg(EEG)
% for data with major DC shifts, this moves all the traces to be centered
% to their mean, so that the high-pass filter struggles less. 

Means = mean(EEG.data, 2);
EEG.data = EEG.data - Means;