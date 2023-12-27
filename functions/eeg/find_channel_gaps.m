function Holes = find_channel_gaps(Data, Chanlocs, Edges)
% identifies in a Ch x T matrix of 1s and 0s any T in which a channel that
% is a 0 does not have a neighboring 1. This therefore finds gaps of
% missing channels that are too big for good interpolation. Edges is all
% the channels that don't need to be considered in this way, because they
% are on the edge of the EEG so are less important and don't have many
% neighbors anyway. Chanlocs is an EEGLAB structure of channel locations.

Dims = size(Data);

% get edges as boolean indexing
Edges = ismember(1:numel(Chanlocs), Edges);

% find all distances between channels
Neighbors = find_neighbors(Chanlocs);

% check that all channels have at least 1 neighbor
nNeighbors = sum(Neighbors);
 if any(nNeighbors(~Edges) < 2)
     warning([num2str(find(nNeighbors(~Edges) < 2)), ' have only 1 neighbor'])
 end

Holes = zeros(1, Dims(2));

for Indx_T = 1:Dims(2) % loop through timepoints

    T = Data(:, Indx_T)';

    % get all 0 channels, excluding Edges
    BadCh = find(T == 0 & ~Edges);

    if numel(BadCh) < 2
        continue
    end

    % loop through those
    for Ch = BadCh

        % get all 1st degree neighbors, and if there are no 1s, then flag this
        % epoch as a hole.
        if ~any(T(Neighbors(Ch, :)))
            Holes(Indx_T) = 1;
            break
        end
    end
end