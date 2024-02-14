function CohenD = cohen_d(Data1, Data2)
% for when dealing with topography. If doing paired; provide already the
% difference values.

nChannels = size(Data1, 2);
CohenD = nan(nChannels, 1);

for ChannelIdx = 1:nChannels
    if exist("Data2", 'var') && ~isempty(Data2) % unpaired
      Table = meanEffectSize(Data1(:, ChannelIdx), Data2(:, ChannelIdx), 'effect','cohen', 'ConfidenceIntervalType','none');
        CohenD(ChannelIdx) = Table.Effect(1);
    else % paired

        if all(isnan(Data1(:, ChannelIdx)))
            continue
        end
      Table = meanEffectSize(Data1(:, ChannelIdx), 'effect','cohen', 'ConfidenceIntervalType','none');

      CohenD(ChannelIdx) = Table.Effect(1);
    end
end

