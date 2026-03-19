# Script And Function Comments

These notes are meant to explain, briefly and concretely, what each local script or function contributes to the paper. I kept them separate from the MATLAB files so the original run scripts stay unchanged.

## Analysis scripts

- [`Analysis/analysisParameters.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/analysisParameters.m): Central analysis configuration: dataset lists, task names, age bins, band definitions, plotting presets, path selection, and statistics defaults.
- [`Analysis/Analysis1_ComputePower.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis1_ComputePower.m): Computes Welch power spectra for each cleaned recording and saves `Power`, `Freqs`, and `EEGMetadata`.
- [`Analysis/Analysis2_DetectBursts.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis2_DetectBursts.m): Detects oscillatory bursts with Matcycle, clusters overlapping bursts across channels, and saves burst-level outputs.
- [`Analysis/Analysis3_AssembleData.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis3_AssembleData.m): Converts per-recording burst and power files into the summary matrices used for manuscript statistics and figures.
- [`Analysis/PolishData.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/PolishData.m): Applies final metadata cleanup, anonymized dataset relabeling, and saves the `ExtendedData_*.mat` files.
- [`Analysis/Figure1_AnalysisGeneology.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure1_AnalysisGeneology.m): Builds the conceptual Figure 1 examples from one participant's cleaned EEG, bursts, and spectra.
- [`Analysis/Figure2_Specta.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure2_Specta.m): Plots age-by-frequency matrices and average spectra showing evening, morning, and overnight differences.
- [`Analysis/Figure3_BasicMixedEffects.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure3_BasicMixedEffects.m): Runs the main mixed-effects models, demographic summaries, age correlations, and sleep-stage associations.
- [`Analysis/Figure4_Figure6_TopographyAverage.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure4_Figure6_TopographyAverage.m): Plots age-group average topographies for whole-range and band-limited measures.
- [`Analysis/Figure5_OvernightTopographies.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure5_OvernightTopographies.m): Fits channel-wise mixed models and plots topographies of overnight change by age.
- [`Analysis/Figure7_OvernightTopographiesBands.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure7_OvernightTopographiesBands.m): Repeats the overnight topography model for band-limited density and related sleep correlations.
- [`Analysis/Figure8_ADHD.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure8_ADHD.m): Fits channel-wise ADHD versus control models and plots the group-effect topographies.
- [`Analysis/QC_IndividualHistograms.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/QC_IndividualHistograms.m): Quality-control visualization of per-participant burst-frequency histograms.
- [`Analysis/ExtendedData_Loaders.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/ExtendedData_Loaders.m): Commented helper snippets showing which exported `ExtendedData_*.mat` file corresponds most closely to each figure script.

## Preprocessing scripts

- [`Preprocessing/prepParameters.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/prepParameters.m): Central preprocessing configuration: raw and output paths, line-noise settings, channel masks, and filter/downsample presets.
- [`Preprocessing/Prep1_Convert2MAT.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep1_Convert2MAT.m): Converts raw EEG folders to EEGLAB `EEG` `.mat` files and fixes a Providence channel-order issue.
- [`Preprocessing/Prep2_Filter.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep2_Filter.m): Creates filtered copies for `ICA`, `Power`, and `Cutting`, merges files within a folder, and assigns anonymized participant codes.
- [`Preprocessing/Prep3_GetICA.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep3_GetICA.m): Removes very noisy data, runs ICA, classifies components with ICLabel, and saves component metadata.
- [`Preprocessing/Prep4_RemoveICA.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep4_RemoveICA.m): Removes non-brain ICA components, performs final window/channel cleaning, interpolates channels, and saves clean power-ready EEG.
- [`Preprocessing/Prep4_ManuallyRemoveICA.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep4_ManuallyRemoveICA.m): Manual ICA review helper for cases where automatic component rejection is not enough.
- [`Preprocessing/Assemble_Metadata.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Assemble_Metadata.m): Builds the combined manuscript metadata table from per-dataset CSV files.

## General metadata and file helpers

- [`functions/general/assemble_metadata.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/assemble_metadata.m): Merges participant, session, and code tables into the unified metadata table.
- [`functions/general/assign_unique_session.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/assign_unique_session.m): Creates a session identifier that is unique across datasets and participants.
- [`functions/general/average_by_column.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/average_by_column.m): Averages rows of data according to a metadata column such as participant or age bin.
- [`functions/general/average_field.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/average_field.m): Replaces a struct field with its across-entry mean.
- [`functions/general/basic_metadata_cleanup.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/basic_metadata_cleanup.m): Standardizes task labels, age bins, session labels, and other metadata fields used across figures.
- [`functions/general/cat_struct.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/cat_struct.m): Concatenates structs safely when one side may be empty.
- [`functions/general/combine_metadata_tables.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/combine_metadata_tables.m): Joins two metadata tables on shared identifiers such as participant and session.
- [`functions/general/gather_folder_paths.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/gather_folder_paths.m): Finds dataset subfolders and participant folders matching the expected raw-data layout.
- [`functions/general/get_outcome_variables.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/get_outcome_variables.m): Returns the manuscript outcome-variable names from a metadata table.
- [`functions/general/list_filenames.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/list_filenames.m): Lightweight folder listing that ignores `.` and `..`.
- [`functions/general/load_datafile.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/load_datafile.m): Loads one participant/session/hour file by matching the filename core and requested variables.
- [`functions/general/load_single_participant.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/load_single_participant.m): Convenience loader for the Figure 1 example participant.
- [`functions/general/make_categorical.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/make_categorical.m): Reorders a metadata column into the category baseline needed for mixed-effects models.
- [`functions/general/match_participants.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/match_participants.m): Matches patient and control participants on age, sex, and task constraints.
- [`functions/general/pair_recordings.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/pair_recordings.m): Builds evening/morning or first/last pairing tables for within-subject comparisons.
- [`functions/general/smooth_frequencies.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/smooth_frequencies.m): Frequency-aware smoothing used in spectra and FOOOF preparation.
- [`functions/general/split_groups.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/split_groups.m): Splits a metadata table into two named groups.
- [`functions/general/unique_metadata.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/unique_metadata.m): Keeps one row per unique value, usually one row per participant.

## EEG processing helpers

- [`functions/eeg/add_cz.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/add_cz.m): Inserts an empty Cz channel before average re-referencing.
- [`functions/eeg/assemble_burst_distributions.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/assemble_burst_distributions.m): Converts burst lists into frequency-binned amplitude and density histograms.
- [`functions/eeg/burst_bands.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/burst_bands.m): Labels each burst by manuscript band membership.
- [`functions/eeg/center_eeg.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/center_eeg.m): Removes large DC offsets by recentering channels.
- [`functions/eeg/channel_distances.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/channel_distances.m): Computes inter-electrode distances from XYZ coordinates.
- [`functions/eeg/channel_slopes.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/channel_slopes.m): Estimates per-channel spectral slopes, used mainly for ICA artifact decisions.
- [`functions/eeg/corr_neighbor_channels.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/corr_neighbor_channels.m): Measures how well each channel agrees with its neighbors.
- [`functions/eeg/fieldtrip2eeglab_events.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/fieldtrip2eeglab_events.m): Renames FieldTrip event fields into EEGLAB-style event structs.
- [`functions/eeg/filter_and_downsample_eeg.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/filter_and_downsample_eeg.m): Applies line filtering, low/high-pass filtering, and resampling using the preset parameter struct.
- [`functions/eeg/find_bad_segments.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/find_bad_segments.m): Detects noisy windows and channels using neighbor correlations and amplitude criteria.
- [`functions/eeg/find_flat_channels.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/find_flat_channels.m): Finds channels that are effectively flat or dead.
- [`functions/eeg/find_iota.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/find_iota.m): Detects an oscillatory peak and reports its status for developmental analyses.
- [`functions/eeg/find_neighbors.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/find_neighbors.m): Builds the neighboring-channel lookup used for artifact checks.
- [`functions/eeg/find_periodic_peak.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/find_periodic_peak.m): Finds the strongest periodic peak within a requested frequency range.
- [`functions/eeg/find_worst_channels.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/find_worst_channels.m): Returns the most poorly correlated channels under a threshold.
- [`functions/eeg/fooof_spectrum.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/fooof_spectrum.m): Runs `fooof()` and returns exponent, offset, periodic power, fit quality, and aperiodic fit.
- [`functions/eeg/highpass_eeg.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/highpass_eeg.m): High-pass FIR helper used to control the transition band explicitly.
- [`functions/eeg/indexes2labels.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/indexes2labels.m): Converts numeric channel indices to EEGLAB labels.
- [`functions/eeg/interpolate_point_channels.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/interpolate_point_channels.m): Interpolates isolated bad channels in point data.
- [`functions/eeg/labels2indexes.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/labels2indexes.m): Converts EEGLAB labels to numeric channel indices.
- [`functions/eeg/line_filter.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/line_filter.m): Removes line frequency and harmonics before later processing.
- [`functions/eeg/manuallyRemoveBadComps.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/manuallyRemoveBadComps.m): GUI-assisted manual selection of ICA components to reject.
- [`functions/eeg/match_sw_amplitudes.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/match_sw_amplitudes.m): Matches two slow-wave amplitude distributions within a tolerance.
- [`functions/eeg/plotComps.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/plotComps.m): Opens the ICA component review interface.
- [`functions/eeg/plot_quick_eeg.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/plot_quick_eeg.m): Fast visual check of EEG traces, with optional ICA overlays.
- [`functions/eeg/simple_fooof_fit.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/simple_fooof_fit.m): Minimal exponent/offset wrapper used during preprocessing.
- [`functions/eeg/top_components_by_category.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/top_components_by_category.m): Converts ICLabel probabilities into one chosen category per component.
- [`functions/eeg/whiten_spectrum.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg/whiten_spectrum.m): Removes the aperiodic fit from a spectrum to isolate periodic power.

## Plot helpers

- [`functions/plots/mixed_model_topography.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/mixed_model_topography.m): Extracts one coefficient from many channel-wise mixed models and plots it as a topography with FDR masking.
- [`functions/plots/plot_age_by_frequency.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/plot_age_by_frequency.m): Draws the age-by-frequency heatmaps used in the manuscript.
- [`functions/plots/plot_clinical.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/plot_clinical.m): Quick clinical-style rereferenced visualization of EEG traces.
- [`functions/plots/plot_highlighted_spectrum.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/plot_highlighted_spectrum.m): Plots a spectrum with manuscript band ranges highlighted.
- [`functions/plots/plot_multicolored_histogram.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/plot_multicolored_histogram.m): Colors burst histograms by theta, alpha, and beta band.
- [`functions/plots/plot_scattercloud.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/plot_scattercloud.m): Scatterplot plus regression summary used in age/outcome panels.
- [`functions/plots/plot_topography_difference.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/plot_topography_difference.m): Computes paired or unpaired topographic contrasts and plots effect size maps.
- [`functions/plots/topo_corner_text.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots/topo_corner_text.m): Places small corner annotations such as `N=`.

## Statistics helpers

- [`functions/stats/cohen_d.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/cohen_d.m): Computes Cohen's d for topographic contrasts.
- [`functions/stats/correct_for_age.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/correct_for_age.m): Removes linear age effects from the outcome variables.
- [`functions/stats/corrtest2_dependent_shared.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/corrtest2_dependent_shared.m): Compares two dependent correlations that share one variable.
- [`functions/stats/disp_demographics.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/disp_demographics.m): Prints compact demographic summaries to the MATLAB console.
- [`functions/stats/disp_mixed_stat.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/disp_mixed_stat.m): Prints one mixed-model coefficient in manuscript-friendly format.
- [`functions/stats/disp_stats_descriptive.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/disp_stats_descriptive.m): Formats descriptive statistics text.
- [`functions/stats/fdr_matrix.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/fdr_matrix.m): Runs FDR correction on a matrix after vectorizing it.
- [`functions/stats/gpt_compare_standardized_coefficients.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/gpt_compare_standardized_coefficients.m): Compares standardized regression coefficients between models.
- [`functions/stats/gpt_vif.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/gpt_vif.m): Computes variance inflation factors from a fitted linear model.
- [`functions/stats/paired_hedges_g.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/paired_hedges_g.m): Paired effect sizes for matrix-form data.
- [`functions/stats/paired_ttest.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/paired_ttest.m): Paired t-tests with effect sizes and FDR correction.
- [`functions/stats/save_model.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/save_model.m): Writes a mixed-model summary to disk.
- [`functions/stats/table_demographics.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/table_demographics.m): Exports demographic tables used in the paper and supplements.
- [`functions/stats/unpaired_hedgesG.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/unpaired_hedgesG.m): Unpaired effect sizes for matrix-form data.
- [`functions/stats/unpaired_ttest.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats/unpaired_ttest.m): Unpaired t-tests with effect sizes and FDR correction.

## Simulation and package helpers

- [`functions/sim/simulate_analysis.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/sim/simulate_analysis.m): Generates simulated spectra and burst-like quantities for method checks.
- [`functions/sim/simulate_recording.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/sim/simulate_recording.m): Simulates one recording's exponent, offset, amplitude, density, and power relationships.
- [`functions/check_packages.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/check_packages.m): Verifies that EEGLAB, chART, and Matcycle are on the MATLAB path.

## EEG import helpers

- [`functions/EGI/load_eeg_data.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/load_eeg_data.m): Main import wrapper for EGI `.raw` and BrainVision `.eeg/.vhdr` files.
- [`functions/EGI/loadEGIBigRaw.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/loadEGIBigRaw.m): Reads the EGI sample matrix from large raw files.
- [`functions/EGI/loadEGIRaw.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/loadEGIRaw.m): Lower-level EGI raw loader returning header and channel data.
- [`functions/EGI/readRAWFileHeader.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/readRAWFileHeader.m): Parses EGI raw-file header information.
- [`functions/EGI/eeg_read_bdf.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/eeg_read_bdf.m): Reads BDF data chunks and header metadata.
- [`functions/EGI/edfread.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/edfread.m): EDF reader bundled with the repo.
- [`functions/EGI/egireadplot.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/egireadplot.m): Legacy plotting example for EGI data inspection.
- [`functions/EGI/brogden.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/brogden.m): Legacy EGI utility script kept with the import code.
- [`functions/EGI/readwritescor.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/readwritescor.m): Legacy EGI score-file helper kept with the import code.

## Vendored external code

- [`functions/external/addExternalFunctions.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/external/addExternalFunctions.m): Adds the bundled third-party helper folders to the MATLAB path.
- [`functions/external/Mass_univariate_erp_toolbox`](/mnt/c/Users/Sophia/Code/children-wake/functions/external/Mass_univariate_erp_toolbox): Vendored ERP statistics toolbox.
- [`functions/external/hhentschke-measures-of-effect-size-toolbox-3d90ae5`](/mnt/c/Users/Sophia/Code/children-wake/functions/external/hhentschke-measures-of-effect-size-toolbox-3d90ae5): Vendored effect-size toolbox.
- [`functions/external/Other`](/mnt/c/Users/Sophia/Code/children-wake/functions/external/Other): Older helper functions for sleep and interpolation workflows.
