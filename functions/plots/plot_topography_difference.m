function plot_topography_difference(Data1, Data2, Chanlocs, CLims, StatParameters, PlotProps)
% plots the t-values (color) and significant channels (white dots) of
% Data2 vs Data1 using chART plots.
% Data are P x Ch matrices.
% Chanlocs is an EEGLAB channel structure.
% CLims is the limits for the colormap. If none is provided, then the
% min/max is used, centered on 0.
% StatsP is a structure with statistics info (see analysisParameters).
% PlotProps is a structure with plotting info (see chART).
% in 2process_Bursts.

%%% Statistics

if isfield(StatParameters, 'Unpaired') && StatParameters.Unpaired
    Stats = unpaired_ttest(Data1, Data2, StatParameters);
else
Stats = paired_ttest(Data1, Data2, StatParameters);
end

ES = Stats.(StatParameters.Paired.ES);
Sig =  Stats.sig;
t_values = Stats.t;

% save max significant Hedge's g, # of sig channels, and # channels with
% G>1
Stats.ES_top1 = nnz(ES >= 1);

ES(Sig==0) = nan; % only consider significant channels for rest
[Stats.ES_maxG, Indx] = max(ES);
Stats.ES_maxGch = Chanlocs(Indx).labels;
Stats.sigtot = nnz(Sig);

if all(isnan(Sig))
    return
end


%%% Plot

% get colorlimits
if isempty(CLims)
    Max = max(abs([quantile(t_values, .01), quantile(t_values, .99)]));
    CLims = [-Max Max];
end

% chART.plot.eeglab_topoplot(Stats.t, Chanlocs, Stats, CLims, 't-values', 'Divergent', PlotProps)

chART.plot.eeglab_topoplot(Stats.hedgesg, Chanlocs, Stats, CLims, 'g-values', 'Divergent', PlotProps)

if PlotProps.Stats.PlotN
    text(.4, .5, ['N=', num2str(Stats.N)], 'FontName', PlotProps.Text.FontName, 'FontSize', PlotProps.Text.LegendSize)
end





