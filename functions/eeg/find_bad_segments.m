function [BadSegments, BadCh, BadWindows_t, Starts, Ends] = ...
    find_bad_segments(EEG, Window, MinNeighborCorrelation, NotEEGChannels, CorrectCz, MinDataKeep, CorrelationFrequencyRange)
% based on correlations with neighboring channels, identifies bad channels
% and timewindows with artefacts. EEG is an EEGLAB structure. Window is in
% seconds the duration of windows to check for bad segments (~4 s),
% MinNeighborCorrelation is how much each channel should correlate to its
% neighbors before being considered an artefact. NotEEGChannels is channels
% that shouldn't correlate with "neighbors" and so won't be considered for
% the preprocessing anyway. CorectCz is a boolean; because of reference,
% this correlation with neighbor's trick doesn't work so well. so to see
% bad channels around Cz, rereference the data, and use those correlation
% values.
% MinDataKeep is for when you don't want to deal with isolated segments,
% and instead just toss out either the entire channel or the entire
% segment. so here you indicate the minimmum amount of data that is
% artefactual that you would still keep as a segment or channel.
% BadWindows_t is a vector the length of the data to indicate which are bad
% timepoints

AmplitudeThreshold = 500; % maximum microvolts before give up entirely on the channel/window

AlternateRef = 16; % different reference to evaluate correlations of channels near the reference
CZPatch = [7 55 106 31 80]; % channels to re-evaluate with different reference

fs = EEG.srate;
[nCh, nPnts] = size(EEG.data);
Chanlocs = EEG.chanlocs;

% filter EEG so correlation not unduely influenced by muscle
if exist('CorrelationFrequencyRange', 'var')
EEG = pop_eegfiltnew(EEG, CorrelationFrequencyRange(1), []);
EEG = pop_eegfiltnew(EEG, [], CorrelationFrequencyRange(2));
end

NotEEGChannels = labels2indexes(NotEEGChannels, Chanlocs); % don't consider not-eeg channels when determining noise
AlternateRefEEG = pop_reref(EEG, AlternateRef);
AlternatePatchIndx = labels2indexes(CZPatch, AlternateRefEEG.chanlocs);
PatchIndx = labels2indexes(CZPatch, Chanlocs);



%%% loop through segments of data to find bad windows
Starts = round(1:Window*fs:nPnts);
Ends = round(Starts+Window*fs-1);
Starts(end) = [];
Ends(end) = [];

BadSegments = zeros(nCh, numel(Starts)); % assign for each channel in each segment a 0 for clean and a 1 for artefact and 2 for unforgivable artefact (really high amplitude)

for Indx_S = 1:numel(Starts)

    % get segment of data
    Data = EEG.data(:, Starts(Indx_S):Ends(Indx_S));

    % correlate it
    Correlations = corr_neighbor_channels(Data, Chanlocs);

    % remove channels that are not EEG
    Correlations(NotEEGChannels, :) = nan;
    Correlations(:, NotEEGChannels) = nan;

    % if cz reference, redo corr for channels around cz
    if exist("CorrectCz", 'var') && CorrectCz
        if mean(Correlations(AlternateRef, :), 'omitnan') > MinNeighborCorrelation % if the alternative reference is a good channel
            AlternateRefData =  AlternateRefEEG.data(:, Starts(Indx_S):Ends(Indx_S));

            % correlate it
            AlternateCorrelations = corr_neighbor_channels(AlternateRefData, AlternateRefEEG.chanlocs);
            Correlations(PatchIndx, PatchIndx) = AlternateCorrelations(AlternatePatchIndx, AlternatePatchIndx);
        else % if alternative reference is bad, then just give up on the whole epoch 
            BadSegments(:, Indx_S) = 1;
            continue
        end
    end

    % find bad channels that arent correlated
    Worst = find_worst_channels(Correlations, MinNeighborCorrelation);
    BadSegments(Worst, Indx_S) = 1;

    % find segments with crazy high amplitudes
    AbsoluteWorst = any(Data>AmplitudeThreshold, 2); % if voltage is too crazy high, then it absolutely has to be removed; some bad segments can otherwise be tolerated
    BadSegments(AbsoluteWorst, Indx_S) = 2;
end


%%% only completely remove segments or not
if exist('MinDataKeep', 'var')
    
    % remove at all costs bad segments with 2s
    AbsoluteBadSegments = remove_channel_or_window(BadSegments==2, 0);

    % remove at all costs bad windows/channels losing more than X% of data
    BadSegments = remove_channel_or_window(BadSegments, MinDataKeep);

    BadSegments(AbsoluteBadSegments) = 1;
end

%%% identify all-bad windows and all-bad channels
BadCh = find(all(BadSegments, 2))';

BadWindows_t = false(1, nPnts);
BadWindows = all(BadSegments, 1);

for Indx_S = 1:numel(Starts)
    if BadWindows(Indx_S)
        BadWindows_t(Starts(Indx_S):Ends(Indx_S)) = 1;
    end
end
end









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions


function R = corr_neighbor_channels(Data, Chanlocs)
% correlates neighboring channels; lets you determine when one channel is
% an outlier.
% Data is a Ch x t matrix

if numel(Chanlocs)==129
    warning('removing CZ')
    Chanlocs(end) = [];
end

M = channel_distances([Chanlocs.X], [Chanlocs.Y], [Chanlocs.Z]);
M(1:numel(Chanlocs)+1:numel(M)) = nan; % set diagonal to nan;
Neighbors = M <= median(min(M))*2; % all channels that are as close as the maximum minimum distance

R = corr(Data');
R(~Neighbors) = nan;
end


function Worst = find_worst_channels(R, Threshold)

Remaining = R;
Worst = [];

while any(mean(Remaining, 'omitnan')<Threshold)
    [~, Indx] = min(mean(Remaining, 'omitnan'));
    Worst = cat(1, Worst, Indx);

    Remaining(Indx, :) = nan;
    Remaining(:, Indx) = nan;
end
end



function BadSegments = remove_channel_or_window(BadSegments, Threshold)
% makes sure that the channel is either completely removed, or the window
% is.

BadSegments = double(BadSegments);
[nCh, nWin] = size(BadSegments);

while any(sum(BadSegments, 2, 'omitnan')/nWin>Threshold) || ...
        any(sum(BadSegments, 1, 'omitnan')/nCh>Threshold) % while there is still missing data to remove in either channels or segments

    % identify amount of missing data for each channel/segment
    PrcntCh = sum(BadSegments, 2, 'omitnan')/nWin;
    PrcntWin = sum(BadSegments, 1, 'omitnan')/nCh;

    % find out which is missing most data
    [MaxCh, IndxCh] = max(PrcntCh);
    [MaxWin, IndxWin] = max(PrcntWin);

    % remove either a channel or a window
    if MaxCh > MaxWin % if the worst channel has more bad data than the worst segment, remove that one
        BadSegments(IndxCh, :) = nan; % nan so that it doesn't interfere with the tally
    else
        BadSegments(:, IndxWin) = nan;
    end
end

BadSegments(isnan(BadSegments)) = 1;
BadSegments = logical(BadSegments);
end



