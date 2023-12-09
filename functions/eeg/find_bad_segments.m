function [BadSegments, BadCh, BadWindows_t, Starts, Ends] = ...
    find_bad_segments(EEG, Window, BadnessThreshold, NotEEG, CorrectCz, AllorNothing)
% Allornothing is a percentage of the data to keep

AmplitudeThreshold = 500; % maximum microvolts before give up entirely on the channel

AlternativeRef = 16;
CZPatch = [7 55 106 31 80];

fs = EEG.srate;
[nCh, nPnts] = size(EEG.data);
Chanlocs = EEG.chanlocs;

NotEEG = labels2indexes(NotEEG, Chanlocs);

Starts = round(1:Window*fs:nPnts);
Ends = round(Starts+Window*fs-1);
Starts(end) = [];
Ends(end) = [];

BadSegments = zeros(nCh, numel(Starts));
rEEG = pop_reref(EEG, AlternativeRef);

for Indx_S = 1:numel(Starts)

    % get segment of data
    Data = EEG.data(:, Starts(Indx_S):Ends(Indx_S));

    % correlate it
    R = corr_neighbor_channels(Data, Chanlocs);

    % remove channels that are not EEG
    R(NotEEG, :) = nan;
    R(:, NotEEG) = nan;

    % if cz reference, redo corr for channels around cz
    if exist("CorrectCz", 'var') && CorrectCz
        if mean(R(AlternativeRef, :), 'omitnan') > BadnessThreshold % if the alternative reference is a good channel
            rData =  rEEG.data(:, Starts(Indx_S):Ends(Indx_S));

            % correlate it
            rR = corr_neighbor_channels(rData, rEEG.chanlocs);
            rPatchIndx = labels2indexes(CZPatch, rEEG.chanlocs);
            PatchIndx = labels2indexes(CZPatch, EEG.chanlocs);

            R(PatchIndx, PatchIndx) = rR(rPatchIndx, rPatchIndx);
        else
            BadSegments(:, Indx_S) = 1;
            continue
        end
    end

    % find worst channels correlated
    Worst = find_worst_channels(R, BadnessThreshold);
    BadSegments(Worst, Indx_S) = 1;

    AbsoluteWorst = any(Data>AmplitudeThreshold, 2);
    BadSegments(AbsoluteWorst, Indx_S) = 2;
end


%%% only completely remove segments or not
if exist('AllorNothing', 'var')
    
    % remove at all costs bad segments with 2s
    AbsoluteBadSegments = remove_channel_or_segment(BadSegments==2, 0);

    % remove at all costs bad windows/channels losing more than X% of data
    BadSegments = remove_channel_or_segment(BadSegments, AllorNothing);

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



function BadSegments = remove_channel_or_segment(BadSegments, Threshold)
% makes sure that the channel is either completely removed, or segent

BadSegments = double(BadSegments);

[nCh, nSeg] = size(BadSegments);

while any(sum(BadSegments, 2, 'omitnan')/nSeg>Threshold) || ...
        any(sum(BadSegments, 1, 'omitnan')/nCh>Threshold) % while there is still missing data to remove in either channels or segments

    % identify amount of missing data for each channel/segment
    PrcntCh = sum(BadSegments, 2, 'omitnan')/nSeg;
    PrcntSeg = sum(BadSegments, 1, 'omitnan')/nCh;

    % find out which is missing most
    [MaxCh, IndxCh] = max(PrcntCh);
    [MaxSeg, IndxSeg] = max(PrcntSeg);

    % remove that one
    if MaxCh > MaxSeg
        BadSegments(IndxCh, :) = nan;
    else
        BadSegments(:, IndxSeg) = nan;
    end

end


BadSegments(isnan(BadSegments)) = 1;
BadSegments = logical(BadSegments);

end
