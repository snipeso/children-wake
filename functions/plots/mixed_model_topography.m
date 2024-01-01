function mixed_model_topography(Models, Chanlocs, CLims, ComparisonString, PlotProps)

Alpha = .05;

nChannels = numel(Chanlocs);

PValue = nan(1, nChannels);
Effect = PValue;
for ChIdx = 1:nChannels
    Model = Models{ChIdx};
    RowIdx = strcmp(Model.Coefficients.Name, ComparisonString);
    PValue(ChIdx) = Model.Coefficients.pValue(RowIdx);
    Effect(ChIdx) = Model.Coefficients.tStat(RowIdx);
end


  [Sig, crit_p, ~, adj_P] = fdr_bh(PValue, Alpha, 'dep');
Stats.sig = Sig;

if isempty(CLims)
    Max = max(abs([quantile(Effect, .01), quantile(Effect, .99)]));
    CLims = [-Max Max];
end

chART.plot.eeglab_topoplot(Effect, Chanlocs, Stats, CLims, "t values", 'Divergent', PlotProps)

DF = Model.Coefficients.DF(1);
topo_corner_text(['df=', num2str(DF)], PlotProps)


% if PlotProps.Stats.PlotN
%     topo_corner_text(['N=', num2str(nParticipants)], PlotProps)
% end