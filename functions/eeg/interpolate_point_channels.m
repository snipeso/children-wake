function Data = interpolate_point_channels(Data, Chanlocs)

Dims = size(Data);

Neighbors = find_neighbors(Chanlocs);

[Amounts, Order] = sort(sum(Neighbors), 'descend'); % go from channel with the most neighbors

for ChannelIdx = Order
    if Amounts(ChannelIdx) < 4 % don't worry about really edge channels
        continue
    end
    for ParticipantIdx = 1:Dims(1)
        Point = Data(ParticipantIdx, ChannelIdx);
        if ~isnan(Point)
            continue
        end

        NeighborData = mean(Data(ParticipantIdx, Neighbors(ChannelIdx, :)), 'omitnan');
        if isnan(NeighborData) % if the gap is so big there's no neighbors without a nan, remove participant
            Data(ParticipantIdx, :) = nan;
            continue
        end
        Data(ParticipantIdx, ChannelIdx) = NeighborData;
    end
end
