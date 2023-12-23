function plot_scattercloud(Table, XColumn, YColumn, PlotProps, ColorgroupColumn, PlotZeroLine, XLim, YLim)
arguments
    Table
    XColumn
    YColumn
    PlotProps = chART.load_plot_properties();
    ColorgroupColumn = '';
    PlotZeroLine = false;
    XLim = [min(Table.(XColumn)), max(Table.(XColumn))];
    YLim = [min(Table.(YColumn)), max(Table.(YColumn))];
end

if ~isempty(ColorgroupColumn)
    Groups = unique(Table.(ColorgroupColumn));
    nGroups = numel(Groups);
else
    ColorgroupColumn = 'DummyIndexes';
    Table.(ColorgroupColumn) = ones(size(Table, 1), 1);
    Groups = 1;
    nGroups = 1;
end

Colors = flip(chART.color_picker(nGroups), 1);


hold on
if PlotZeroLine
    plot(XLim, [0 0], ':', 'Color', 'k', 'HandleVisibility','off')
end

for GroupIdx = 1:nGroups
    Indexes = ismember(Table.(ColorgroupColumn), Groups(GroupIdx));
    scatter(Table.(XColumn)(Indexes), Table.(YColumn)(Indexes), PlotProps.Scatter.Size, ...
       'MarkerFaceColor', Colors(GroupIdx,:), ...
        'MarkerEdgeColor','none', ...
        'MarkerFaceAlpha', PlotProps.Scatter.Alpha)
    colormap(PlotProps.Color.Maps.Linear)
end
chART.set_axis_properties(PlotProps)

xlim(XLim)
ylim(YLim)


[R, p] = corr(Table.(XColumn),  Table.(YColumn));

if p<.05

h = lsline;

for GroupIdx = 1:numel(Groups)
    set(h(GroupIdx),'color', Colors(numel(Groups)+1-GroupIdx,:), 'LineWidth', PlotProps.Line.Width)
end

if R>0
    Start = XLim(1);
    HA = 'left';
else
    Start = XLim(2);
    HA = 'right';
end
text(Start, YLim(end), ['r=', num2str(R, '%.2f')], ...
    'FontName', PlotProps.Text.FontName, 'FontSize', PlotProps.Text.AxisSize, 'HorizontalAlignment', HA)
chART.utils.pad_axis('y', .05)
chART.utils.pad_axis('x', .05)
end

if nGroups>1
    legend(Groups)
end

