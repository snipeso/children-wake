% figure for illustrating results
Parameters = analysisParameters();
PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Text.TitleSize = 14;
PlotProps.Axes.xPadding = 15;
PlotProps.Axes.yPadding = 15;
PlotProps.Figure.Padding = 30;

%%
close all

Grid = [2 2];

Colors = chART.color_picker([2 2]);
KidMorning = squeeze(Colors(1, :, 2));
KidEvening = squeeze(Colors(1, :, 1));
AdultMorning = squeeze(Colors(2, :, 2));
AdultEvening = squeeze(Colors(2, :, 1));
LW = 5;
Legend = {'Evening', 'Morning'};

%%% slope change
X = [1 2];
YKidEvening = [10, 7];
YKidMorning = [YKidEvening(1)-0.5, YKidEvening(2)-3.5];
YAdultEvening =  [YKidEvening(1)-5.5, YKidEvening(2)-4];
YAdultMorning =  [YAdultEvening(1)-.5, YAdultEvening(2)-2];
figure('Units','centimeters','InnerPosition',[0 0 12 12])
plot_spectrogram([1 1], PlotProps)
plot(X, YKidEvening, 'Color', KidEvening, 'LineWidth',LW) % kid evening
plot(X, YKidMorning, 'Color', KidMorning, 'LineWidth',LW) % kid morning
title('Children')
vertical_text('Aperiodic', PlotProps)


plot_spectrogram([1 2], PlotProps)
plot(X, YAdultEvening, 'Color', AdultEvening, 'LineWidth',LW) % adult evening
plot(X, YAdultMorning, 'Color', AdultMorning, 'LineWidth',LW) % adult morning
legend(Legend)
title('Adults')


%%% bursts change


function plot_spectrogram(Position, PlotProps)
Grid = [2 2];
chART.sub_plot([], Grid, Position, [], true, '', PlotProps);
ylim([0 10])
chART.set_axis_properties(PlotProps)
hold on
axis square
xlabel('Frequency (log)')
set(gca, 'YTick', '', 'XTick', '')

if Position(2)==1
    ylabel('Power (log)')
end
end


function vertical_text(Text, PlotProps)
X = get(gca, 'XLim');
Y = get(gca, 'YLim');
text(X(1)-diff(X)*.3, Y(1)+diff(Y)*.5, Text, ...
    'FontSize', PlotProps.Text.TitleSize, 'FontName', PlotProps.Text.FontName, ...
    'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
end