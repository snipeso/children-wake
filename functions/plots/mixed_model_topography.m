function mixed_model_topography(Models, ColorParameter, Coefficient, Chanlocs, CLims, PlotProps, ColorLabel)
% plots topographies from mixed effects models, whichever statistic is
% specified in ColorParameters, and whichever estimate is specified by the
% coefficient (e.g. 'Age', or 'Age:Hour_2').

Alpha = .05;

nChannels = numel(Chanlocs);

if ~exist("ColorLabel", 'var')
switch ColorParameter
    case 'tStat'
        ColorLabel = 't-values';
    case 'Estimate'
        ColorLabel = '\beta';
end
end

PValue = nan(1, nChannels);
Effect = PValue;
for ChIdx = 1:nChannels
    Model = Models{ChIdx};
    RowIdx = strcmp(Model.Coefficients.Name, Coefficient);
    PValue(ChIdx) = Model.Coefficients.pValue(RowIdx);
    Effect(ChIdx) = Model.Coefficients.(ColorParameter)(RowIdx);
end


% Stats.sig = PValue < Alpha; % debug without fdr correction
[~, p_masked] = fdr(PValue, Alpha);
Stats.sig = p_masked;

if isempty(CLims)
    Max = max(abs([quantile(Effect, .01), quantile(Effect, .99)]));
    CLims = [-Max Max];
end

chART.plot.eeglab_topoplot(Effect, Chanlocs, Stats, CLims, ColorLabel, 'Divergent', PlotProps)

DF = Model.Coefficients.DF(1);
topo_corner_text(['df=', num2str(DF)], PlotProps)
