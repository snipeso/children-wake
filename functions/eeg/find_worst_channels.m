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
