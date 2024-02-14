function R = corr_neighbor_channels(Data, Chanlocs)
% correlates neighboring channels; lets you determine when one channel is
% an outlier.
% Data is a Ch x t matrix

if numel(Chanlocs)==129
    warning('removing CZ')
    Chanlocs(end) = [];
end

 Neighbors = find_neighbors(Chanlocs);

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
