function FlatChannels = find_flat_channels(EEG)
EEGHyperclean = clean_artifacts(EEG, ...
    'Highpass', 'off', ...
    'ChannelCriterion', 'off', ...
    'LineNoiseCriterion', 'off', ...
    'BurstRejection', 'off',...
    'BurstCriterion', 'off', ...
    'BurstCriterionRefMaxBadChns', 'off', ...
    'WindowCriterion', 'off');

FlatChannels = str2double(setdiff({EEG.chanlocs.labels}, {EEGHyperclean.chanlocs.labels}));
end