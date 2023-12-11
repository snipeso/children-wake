NEEG = clean_artifacts(NewEEG, ...
    'FlatlineCriterion', 'off', ...
    'Highpass', 'off', ...
    'ChannelCriterion', 'off', ...
    'LineNoiseCriterion', 'off', ...
    'BurstRejection', 'off',...
    'BurstCriterion', 'off', ...
    'BurstCriterionRefMaxBadChns', 'off', ...
    'WindowCriterion', .3); % fairly lax remove bad data



nolocs_channel_crit =    0.4500;

nolocs_channel_crit_excluded =    0.1000;


channel_crit_maxbad_time =    0.5000;


[NEEG,removed_channels] = clean_channels_nolocs(NewEEG,nolocs_channel_crit,nolocs_channel_crit_excluded,[],channel_crit_maxbad_time);




window_crit =  0.3000;
window_crit_tolerances = [-Inf     12];
EEG = clean_windows(NewEEG,window_crit,window_crit_tolerances);


[ewEEG,removed_channels] = clean_channels_nolocs(NewEEG,...
                MinCorrelation,NoLocsChannelCritExcluded,[],ChannelCriteriaMaxBadTime);




%%
[BadSegments, BadCh, BadWindows_t, Starts, Ends] = ...
    find_bad_segments(NewEEG, WindowLength, 0, EEG_Channels.notEEG, false, MinDataKeep, CorrelationFrequencyRange, 150);
figure;imagesc(BadSegments)