# ProcessedData.mat

This file is the publication-ready version of the wake EEG cache. It is written in [`Analysis/Analysis8_PolishDataForPublication.m`](./Analysis/Analysis8_PolishDataForPublication.m), which loads the main cache from `AllBursts.mat`, cleans `Metadata`, recodes dataset names, renames several metadata outcome columns for publication, and then saves the variables listed below.

The underlying data assembly happens in [`Analysis/Analysis2_Assemble_Data.m`](./Analysis/Analysis2_Assemble_Data.m). In that script, each row corresponds to one recording-task combination (`NewIdx`), and that same first dimension is shared across `Metadata`, `BurstInformationTopography`, `BurstInformationTopographyBands`, `BurstInformationClusters`, and `AverageSpectrograms`. In practice, row `i` in `Metadata` matches row `i` in every exported data matrix.

## Figure usage summary

- Figure 1: does not use `ProcessedData.mat`; it loads a single example participant directly in [`Analysis/Figure1_AnalysisGeneology.m`](./Analysis/Figure1_AnalysisGeneology.m).
- Figure 2: uses `Metadata`.
- Figure 3: uses `Metadata`, `BurstInformationTopography`, `Chanlocs`.
- Figure 4: uses `Metadata`, `BurstInformationTopography`, `Chanlocs`.
- Figure 5: uses `Metadata`, `BurstInformationClusters`, `Frequencies`, `AverageSpectrograms`, `AllFrequencies`.
- Figure 6: uses `Metadata`, `BurstInformationTopographyBands`, `Chanlocs`. Figure 6 is generated inside [`Analysis/Figure3_Figure6_TopographyAverage.m`](./Analysis/Figure3_Figure6_TopographyAverage.m).
- Figure 7: uses `Metadata`, `BurstInformationTopographyBands`, `Chanlocs`.
- Figure 8: uses `Metadata`, `BurstInformationTopography`, `Chanlocs`.

## Variables

### `Metadata`

What it is:
One row per recording-task combination, carrying participant/session information plus recording-level summary measures. The per-row task expansion is created in `Analysis2_Assemble_Data` at lines 76-118 and finalized by replacing the original table with `TaskMetadata` at lines 248-249. Recording-level summary measures are filled from burst clusters and average power/FOOOF outputs at lines 114-128 and 200-208.

Important summary columns created in `Analysis2_Assemble_Data`:

- `Amplitude`: mean burst-cluster amplitude for that recording-task.
- `Globality`: mean cluster globality, expressed as percent.
- `Duration`: mean burst duration in seconds.
- `Quantity`: percent of the recording occupied by bursts.
- `Power`: mean log10 power across non-edge channels in the 4-15 Hz analysis range.
- `PeriodicPower`: mean whitened/periodic power across the same analysis range.
- `AperiodicPower`: mean aperiodic fit power across the same analysis range.
- `Slope`: FOOOF aperiodic exponent estimate from the recording-average spectrum.
- `Intercept`: FOOOF aperiodic offset estimate from the recording-average spectrum.
- `Error`: FOOOF fit error.
- `RSquared`: FOOOF fit R^2.

Publication rename step:
In `Analysis8_PolishDataForPublication` lines 45-49, the outcome columns are renamed for publication using `Parameters.OutcomeMeasures.Titles`:

- `Quantity` -> `Density`
- `Slope` -> `Exponent`
- `Intercept` -> `Offset`
- `PeriodicPower` remains `PeriodicPower` after `genvarname('Periodic power')`

Where it is used:

- Figure 2: loaded in `Analysis/Figure2_BasicMixedEffects.m:37`; used for mixed-effects models (`:64-115`), age scatterplots (`:146-204`), and sleep-stage correlations/models (`:212-258`).
- Figure 3: loaded in `Analysis/Figure3_Figure6_TopographyAverage.m:32-33`; filtered by age/task and used to average participants within age groups (`:36-40`, `:73-79`).
- Figure 4: loaded in `Analysis/Figure4_OvernightTopographies.m:35-37`; used to define age/task subsets and mixed models of overnight change (`:44-78`).
- Figure 5: loaded in `Analysis/Figure5_Spectrogram.m:30`; used to form oddball subsets, age bins, and paired evening-morning data (`:32-35`, `:54-55`, `:112-167`).
- Figure 6: same script as Figure 3; used to group and average band-specific topographies (`Analysis/Figure3_Figure6_TopographyAverage.m:123-144`).
- Figure 7: loaded in `Analysis/Figure7_OvernightTopographiesBands.m:35-37`; used for age/task subsets, mixed models, and sleep-correlation analyses (`:46-80`, `:143-160`, `:184-202`).
- Figure 8: loaded in `Analysis/Figure8_ADHD.m:36-38`; used as the model table for ADHD vs HC topography statistics (`:41-56`).

### `BurstInformationTopography`

What it is:
A struct of channel-level topographies across the full wake analysis frequency range (4-15 Hz after removing the upper edge). It is initialized in `Analysis2_Assemble_Data` at lines 67-72 and filled channel-by-channel at lines 168-193.

Fields and meanings:

- `Amplitude`: mean burst amplitude at each channel, across all bursts in the analysis range. Set to `NaN` if fewer than `MinBursts` bursts were available at that channel.
- `Quantity`: percent of the recording occupied by bursts at that channel.
- `Power`: mean log10 power at that channel across the 4-15 Hz analysis range.
- `Slope`: channel-wise FOOOF aperiodic exponent.
- `Intercept`: channel-wise FOOOF aperiodic offset.
- `PeriodicPower`: channel-wise mean whitened/periodic power across the 4-15 Hz analysis range.

Expected shape:
Each field is `nRecordings x 123`.

Where it is used:

- Figure 3: `Analysis/Figure3_Figure6_TopographyAverage.m:69-83` plots age-group average topographies from `BurstInformationTopography.(Measures{MeasureIdx})`.
- Figure 4: `Analysis/Figure4_OvernightTopographies.m:57-78` uses `BurstInformationTopography.(Measures{MeasureIdx})` as the dependent variable for channel-wise mixed models; Figure 4 topoplots are rendered at `:104-123`.
- Figure 8: `Analysis/Figure8_ADHD.m:50-57` uses `BurstInformationTopography.(Measures{MeasureIdx})` for channel-wise ADHD vs HC models; Figure 8 topoplots are rendered at `:83-95`.

### `BurstInformationTopographyBands`

What it is:
A struct of channel-level topographies split into canonical bands from `Parameters.Bands`: Theta 4-7 Hz, Alpha 8-11 Hz, and Beta 12-16 Hz. It is initialized in `Analysis2_Assemble_Data` at lines 57-65 and filled at lines 134-161.

Fields and meanings:

- `Quantity`: percent of the recording occupied by bursts in that channel and band.
- `Amplitude`: mean burst amplitude in that channel and band, set to `NaN` when fewer than `MinBursts` bursts are available.
- `Power`: mean log10 power in that channel and band.
- `PeriodicPower`: mean whitened/periodic power in that channel and band.

Expected shape:
Each field is `nRecordings x 123 x 3`, where the third dimension is band order `[Theta, Alpha, Beta]`.

Where it is used:

- Figure 6: `Analysis/Figure3_Figure6_TopographyAverage.m:115-148` averages and plots `BurstInformationTopographyBands.(Measures{MeasureIdx})` by age group and band.
- Figure 7: `Analysis/Figure7_OvernightTopographiesBands.m:59-80` uses `BurstInformationTopographyBands.(WakeMeasure)` for band-specific mixed models, and `:102-127` plots the resulting overnight-change topographies.
- Additional Figure 7 analyses in the same script correlate band topographies with sleep measures using `BurstInformationTopographyBands.(WakeMeasure)` at `:157-160` and `:200-202`.

### `BurstInformationClusters`

What it is:
A struct of frequency-binned recording-level summaries, averaged across channels/non-edge channels as appropriate. It is initialized in `Analysis2_Assemble_Data` at lines 48-54 and filled in the frequency loop at lines 210-241.

Fields and meanings:

- `Amplitude`: mean burst-cluster amplitude within each frequency bin.
- `Quantity`: percent of the recording occupied by burst clusters within each frequency bin.
- `Duration`: mean burst-cluster duration within each frequency bin.
- `Globality`: mean cluster globality within each frequency bin, expressed as percent.
- `Power`: mean log10 power across non-edge channels within each frequency bin.
- `PeriodicPower`: mean whitened/periodic power within each frequency bin.

Expected shape:
Each field is `nRecordings x 12`. The 12 bins come from `Frequencies = 4:16` in the assembly script, followed by removing the last edge before saving.

Where it is used:

- Figure 5: `Analysis/Figure5_Spectrogram.m:47-68` uses `BurstInformationClusters.(Measures{MeasureIdx})` to plot age-by-frequency surfaces for evening recordings.
- Figure 5: `Analysis/Figure5_Spectrogram.m:82-103` uses the same variables to compute and plot overnight change surfaces.

### `Frequencies`

What it is:
The frequency-bin labels for `BurstInformationClusters`. In the assembly script it begins as bin edges `4:16` (`Analysis2_Assemble_Data.m:13`), is used for discretization (`:211-214`), and then the last edge is removed before saving (`:249`). The saved vector therefore labels the lower edge / plotting position for the 12 retained bins.

This is label information in service of:

- `BurstInformationClusters`

Expected shape:
`1 x 12` or `12 x 1`, depending on MATLAB load context.

Where it is used:

- Figure 5: `Analysis/Figure5_Spectrogram.m:67` and `:97` pass `Frequencies` into `plot_age_by_frequency(...)` as the y-axis for the frequency-binned cluster summaries.

### `Chanlocs`

What it is:
The EEGLAB channel-location struct for the 123 scalp channels used in the wake analyses. It is taken from `EEGMetadata.chanlocs` during assembly (`Analysis2_Assemble_Data.m:99-101`) and saved so the topographic arrays can be plotted in sensor space.

This is label/geometry information in service of:

- `BurstInformationTopography`
- `BurstInformationTopographyBands`

Expected shape:
EEGLAB struct array with 123 elements.

Where it is used:

- Figure 3: `Analysis/Figure3_Figure6_TopographyAverage.m:83` and `:148` use `Chanlocs` for topoplot rendering.
- Figure 4: `Analysis/Figure4_OvernightTopographies.m:110-111` uses `Chanlocs` in `mixed_model_topography(...)`.
- Figure 7: `Analysis/Figure7_OvernightTopographiesBands.m:107`, `:166`, and `:208` use `Chanlocs` for mixed-model and correlation topoplots.
- Figure 8: `Analysis/Figure8_ADHD.m:86-87` uses `Chanlocs` for ADHD effect topoplots.

### `AllFrequencies`

What it is:
The full frequency axis returned with the power spectra loaded from disk (`Analysis2_Assemble_Data.m:121-125`). It is used to locate frequency ranges for computing band/full-range power (`:127`, `:139`, `:184`), to define the FOOOF/whitened power index ranges (`:145`, `:192`, `:204`), and to label the saved average spectra (`:198`).

This is label information in service of:

- `AverageSpectrograms`

It also underlies the calculation of:

- `BurstInformationTopography.Power`
- `BurstInformationTopography.PeriodicPower`
- `BurstInformationTopographyBands.Power`
- `BurstInformationTopographyBands.PeriodicPower`
- `Metadata.Power`
- `Metadata.PeriodicPower`
- `Metadata.AperiodicPower`

Expected shape:
Frequency vector matching the second dimension of `AverageSpectrograms` and the loaded PSD arrays. In `Analysis2_Assemble_Data`, `AverageSpectrograms` is preallocated with 513 columns, so this is expected to have length 513.

Where it is used:

- Figure 5: `Analysis/Figure5_Spectrogram.m:145` and `:171` pass `AllFrequencies` into `plot_spectrogram(...)` as the x-axis for the full average spectra.

### `AverageSpectrograms`

What it is:
The average power spectrum for each recording-task, computed as the mean PSD across the non-edge channels (`Analysis2_Assemble_Data.m:196-198`). Despite the variable name, this is a 1D average spectrum per recording, not a time-resolved spectrogram.

Expected shape:
`nRecordings x 513`.

Where it is used:

- Figure 5 supplementary panels: `Analysis/Figure5_Spectrogram.m:137-145` averages `AverageSpectrograms` within age groups and plots evening vs morning spectra using `AllFrequencies`.
- Figure 5 supplementary task panels: `Analysis/Figure5_Spectrogram.m:163-171` averages `AverageSpectrograms` within task and hour, again plotted against `AllFrequencies`.

## Notes

- `ProcessedData.mat` contains only the variables saved in `Analysis8_PolishDataForPublication.m:56-57`.
- Figure scripts in the repository usually load `AllBursts.mat`, but the variable names and structure match the exported `ProcessedData.mat`. The main publication-facing difference is that `Metadata` has been cleaned and some outcome columns were renamed.
