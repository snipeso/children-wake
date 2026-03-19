function topo_corner_text(Text, PlotProps)
% Places small corner annotations such as `N=`.

text(.4, .5, Text, 'FontName', PlotProps.Text.FontName, 'FontSize', PlotProps.Text.LegendSize)
