function plot_highlighted_spectrum(Power, Freqs, Bands, PlotProps)

% Colors = flip(chART.color_picker(3, 'rainbow'));
Colors = chART.color_picker(11, 'rainbow');
Colors = flip(Colors(1:3, :));

BandLabels = fieldnames(Bands);

hold on

plot(Freqs, Power, 'Color', 'k', 'LineWidth',PlotProps.Line.Width, 'HandleVisibility','off')
chART.set_axis_properties(PlotProps)

% for BandIdx = 1:numel(BandLabels)
for BandIdx = numel(BandLabels):-1:1
    Band = dsearchn(Freqs', Bands.(BandLabels{BandIdx})');
    area(Freqs(Band(1):Band(2)), Power(Band(1):Band(2)), 'BaseValue',min(Power), ...
        'FaceColor', Colors(BandIdx, :), 'FaceAlpha', .5, ...
        'LineStyle', 'none')
    plot(Freqs(Band(1):Band(2)), Power(Band(1):Band(2)), ...
        'Color', Colors(BandIdx, :), 'LineWidth', PlotProps.Line.Width, 'HandleVisibility','off')
end
legend(flip(BandLabels))
    set(legend, 'ItemTokenSize', [10 10], 'location', 'northeast')

