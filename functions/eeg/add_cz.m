function EEG = add_cz(EEG)
% adds an empty channel for Cz

if any(strcmpi({EEG.chanlocs.labels}, 'CZ'))
    return
end

load('Cz.mat', 'CZ')

EEG.data(end+1, :) = zeros(1, size(EEG.data, 2));
EEG.chanlocs(end+1) = CZ;
EEG = eeg_checkset(EEG);