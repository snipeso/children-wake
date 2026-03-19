% ExtendedData_Loaders.m
% Commented helper snippets for loading the manuscript's exported
% ExtendedData MAT files without editing the original figure scripts.
%
% These are not meant to replace the original cache exactly. They just show
% the closest exported file to load for each figure family.

%% Common setup

% Parameters = analysisParameters();
% Paths = Parameters.Paths;
% ResultsFolder = fullfile(Paths.Results, 'MainStatsStandardized');

%% Figure 2 and Figure 3 style summary data
% ExtendedData_2.mat contains the cleaned task-level metadata and the mean
% spectra that were exported for sharing.

% load(fullfile(ResultsFolder, 'ExtendedData_2.mat'), ...
%     'Metadata', 'SpectraAverage', 'Frequencies')
%
% Metadata = basic_metadata_cleanup(Metadata);
%
% % Notes:
% % - This is the closest export for Figure2_Specta.m and
% %   Figure3_BasicMixedEffects.m.
% % - Figure2_Specta.m still expects SpectraRedux and FrequenciesRedux,
% %   which are only present in the full ProcessedData.mat cache.
% % - Figure3_BasicMixedEffects.m can reuse Metadata directly for many of
% %   the manuscript statistics.

%% Figure 4, Figure 5, and Figure 8 topographies
% ExtendedData_4.mat contains the all-frequency channel summaries.

% load(fullfile(ResultsFolder, 'ExtendedData_4.mat'), ...
%     'Metadata', 'Topographies', 'Chanlocs')
%
% Metadata = basic_metadata_cleanup(Metadata);
%
% % Closest match for:
% % - Figure4_Figure6_TopographyAverage.m (all-frequency panels)
% % - Figure5_OvernightTopographies.m
% % - Figure8_ADHD.m

%% Figure 6 and Figure 7 band-limited topographies
% ExtendedData_6.mat contains the band-resolved channel summaries.

% load(fullfile(ResultsFolder, 'ExtendedData_6.mat'), ...
%     'Metadata', 'TopographiesBands', 'Chanlocs', 'Bands')
%
% Metadata = basic_metadata_cleanup(Metadata);
%
% % Closest match for:
% % - Figure4_Figure6_TopographyAverage.m (band panels)
% % - Figure7_OvernightTopographiesBands.m

%% Figure 1 example recording
% Figure1_AnalysisGeneology.m uses one participant's cleaned EEG, bursts,
% and power files directly rather than an ExtendedData export.
% There is no matching ExtendedData MAT file for that figure in this repo.

%% QC histogram script
% QC_IndividualHistograms.m also depends on per-recording burst files, not
% on one of the exported ExtendedData MAT files.
