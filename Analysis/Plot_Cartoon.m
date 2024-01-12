% figure for illustrating results
Parameters = analysisParameters();
Paths = Parameters.Paths;
PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Text.TitleSize = 14;
PlotProps.Axes.xPadding = 15;
PlotProps.Axes.yPadding = 15;
PlotProps.Figure.Padding = 30;

ResultsFolder = fullfile(Paths.Results, 'Main');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

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
YKidEvening = [10, 5.5];
YKidMorning = [YKidEvening(1)-1, YKidEvening(2)-3.5];
YAdultEvening =  [YKidEvening(1)-5.5, YKidEvening(2)-3];
YAdultMorning =  [YAdultEvening(1)-1, YAdultEvening(2)-2];

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
    set(legend, 'ItemTokenSize', [20 20], 'location', 'northeast')
title('Adults')


plot_time([2 1], PlotProps)
vertical_text('Periodic', PlotProps)
 [Wave, t] = generate_waves(8, .15);
plot(t, Wave+16, 'Color', KidEvening, 'LineWidth',LW)

 [Wave, t] = generate_waves(4, .25);
plot(t, Wave, 'Color', KidMorning, 'LineWidth',LW)

plot_time([2 2], PlotProps)
 [Wave, t] = generate_waves(4.5, .25);
plot(t, Wave+16, 'Color', AdultEvening, 'LineWidth',LW)

 [Wave, t] = generate_waves(2.25, .15);
plot(t, Wave, 'Color', AdultMorning, 'LineWidth',LW)
chART.save_figure('Cartoon', ResultsFolder, PlotProps)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

function plot_time(Position, PlotProps)
Grid = [2 2];
chART.sub_plot([], Grid, Position, [], true, '', PlotProps);
ylim([-10 30])
chART.set_axis_properties(PlotProps)
hold on
axis square
xlabel('Time')
set(gca, 'YTick', '', 'XTick', '')

if Position(2)==1
    ylabel('Amplitude')
end
end


function vertical_text(Text, PlotProps)
X = get(gca, 'XLim');
Y = get(gca, 'YLim');
text(X(1)-diff(X)*.3, Y(1)+diff(Y)*.5, Text, ...
    'FontSize', PlotProps.Text.TitleSize, 'FontName', PlotProps.Text.FontName, ...
    'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
end


function [Wave, t] = generate_waves(Amplitude, Fraction)
TotalWaves = 5;
Pnts = 500;
t = linspace(0, 1, Pnts);
Wave = Amplitude*cos(2*pi*t*TotalWaves);
Wave(round(1:Pnts*Fraction)) = 0;
Wave(end-round(Pnts*Fraction):end) = 0;
end