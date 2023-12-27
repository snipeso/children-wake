function Neighbors = find_neighbors(Chanlocs)

M = channel_distances([Chanlocs.X], [Chanlocs.Y], [Chanlocs.Z]);
M(1:numel(Chanlocs)+1:numel(M)) = nan; % set diagonal to nan;
Neighbors = M <= median(min(M))*2; % all channels that are as close as the maximum minimum distance

