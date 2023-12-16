function plot_age_by_frequency(Data, Ages, Frequencies, Colormap, Label, PlotProps)
% Data is an age x frequency matrix

contourf(Ages, Frequencies, Data', 100, 'linecolor','none')
chART.set_axis_properties(PlotProps)
colormap(PlotProps.Color.Maps.(Colormap))

xticks(Ages)
yticks(5:2:15)
h = colorbar;
h.TickLength = 0;
ylabel(h, Label, 'FontName', PlotProps.Text.FontName, 'FontSize', PlotProps.Text.AxisSize) % text style needs to be specified for label, because its weird

if strcmp(Colormap, 'Divergent')
    C = clim;
    Max = max(abs(C));
    clim([-Max, Max])
end