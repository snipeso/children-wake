function EEG = preprocess_eeg(EEG, Parameters)
% Parameters should have .fs, .lp, .hp, .hp_stopband, .line

% set selected parameters
new_fs = Parameters.fs;
lowpass = Parameters.lp;
highpass = Parameters.hp;
hp_stopband = Parameters.hp_stopband;
line_noise = Parameters.line;


% center each channel to its mean
EEG = center_eeg(EEG);

% low-pass filter
EEG = pop_eegfiltnew(EEG, [], lowpass); % this is a form of antialiasing, but it not really needed because usually we use 40hz with 256 srate

% notch filter for line noise
EEG = line_filter(EEG, line_noise, false);

% resample
if EEG.srate ~= new_fs
    EEG = pop_resample(EEG, new_fs);
end

% high-pass filter
% NOTE: this is after resampling, otherwise crazy slow.
% NOTE2: also before merging, because of crazy drifts
EEG = highpass_eeg(EEG, highpass, hp_stopband);