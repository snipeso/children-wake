function plot_multicolored_histogram(Histogram, Frequencies, Bands, PlotProps)

Width = .8;
Colors = chART.color_picker(11, 'rainbow');
Colors = flip(Colors(1:3, :));

BandLabels = fieldnames(Bands);
Remaining = true(size(Frequencies));

hold on
for BandIdx = 1:numel(BandLabels)
    Band = dsearchn(Frequencies', Bands.(BandLabels{BandIdx})');
Temp = zeros(size(Histogram));
Temp(Band(1):Band(2)) = Histogram(Band(1):Band(2));
    % bar(Frequencies, Temp, Width, ...
    %     'EdgeColor', Colors(BandIdx, :), 'FaceColor', Colors(BandIdx, :), 'FaceAlpha', .5)
        bar(Frequencies, Temp, Width, ...
        'EdgeColor', 'none', 'FaceColor', Colors(BandIdx, :), 'FaceAlpha', .6)
    Remaining(Band(1):Band(2)) = 0;
end

chART.set_axis_properties(PlotProps)
Temp = zeros(size(Histogram));
Temp(Remaining) = Histogram(Remaining);
bar(Frequencies, Temp, Width, 'FaceColor', 'none', 'EdgeColor', [.8 .8 .8], 'FaceAlpha', .2)
