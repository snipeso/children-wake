# Wake Oscillations In Children

This repository contains the MATLAB preprocessing, analysis, and figure code for the wake EEG paper from *The interaction between sleep and developent on wake EEG oscillations* (Snipes et al. 2026). The code pools several child, adolescent, and adult datasets recorded in the evening before sleep and the morning after sleep, then quantifies oscillation amplitude, oscillation density, spectral power, and aperiodic measures.

## What This Repo Produces

The pipeline is:

1. Convert raw EEG recordings to EEGLAB `EEG` structs.
2. Create filtered copies for ICA, power analysis, and manual inspection.
3. Run ICA and remove artifactual components.
4. Compute Welch power spectra.
5. Detect oscillatory bursts with Matcycle.
6. Assemble per-recording, per-frequency, and per-channel summary variables.
7. Save manuscript-ready caches and `ExtendedData_*.mat` files.

The main analysis outputs are:

- `EEG`:
  Preprocessed continuous EEG in EEGLAB format.
- `Power`, `Freqs`, `Duration`, `Chanlocs`, `EEGMetadata`:
  Per-recording spectral output from [`Analysis/Analysis1_ComputePower.m`](/Analysis/Analysis1_ComputePower.m).
- `Bursts`, `BurstClusters`, `EEGMetadata`:
  Per-recording burst detections from [`Analysis/Analysis2_DetectBursts.m`](/Analysis/Analysis2_DetectBursts.m).
- `Metadata`:
  Task-level summary table assembled in [`Analysis/Analysis3_AssembleData.m`](/Analysis/Analysis3_AssembleData.m).
- `SpectraAverage`:
  Mean power spectrum across non-edge channels for each recording.
- `SpectraRedux`:
  Frequency-binned summary matrices for amplitude, density, power, and periodic power.
- `Topographies`:
  Per-channel summaries collapsed across 4-16 Hz.
- `TopographiesBands`:
  Per-channel summaries split into Theta, Alpha, and low Beta bands.
- `Frequencies`, `FrequenciesRedux`, `Chanlocs`, `Bands`:
  Axes and channel metadata used by the figure scripts.


## Required Repositories And Toolboxes

The analysis code depends on sibling repositories and MATLAB toolboxes that are not fully contained here.

### Sibling repositories

Make sure matlab knows the path to these other repositories:

- `../Matcycle`
  Provides the `cycy.*` functions used for Welch power and burst detection.
- `../chART`
  Provides plotting defaults and figure helpers.
- `../eeglab2023.0`
    from [EEGLAB](https://eeglab.org/)
- `../fooof_mat`
  Local MATLAB `fooof()` implementation visible in this workspace.


### MATLAB toolboxes and external packages

Needed or strongly implied by the code:

- EEGLAB
  Required throughout preprocessing and topographic plotting.
- EEGLAB plugins:
  `ICLabel`, `clean_rawdata`, and BrainVision import support if you use `.vhdr/.eeg` files.
- FieldTrip (can use the light scripts that come with eeglab)
  Used in [`functions/EGI/load_eeg_data.m`](/functions/EGI/load_eeg_data.m) to read trigger events from EGI raw files.
- `fooof_mat` / specparam MATLAB wrapper
  Required because the analysis calls `fooof()` directly.
- Statistics and Machine Learning Toolbox
  Needed for `fitlme`, `fitlm`, correlations, and t-tests.
- Signal Processing Toolbox
  Needed for `pwelch`, `designfilt`, `filtfilt`, and related filtering.
- Parallel Computing Toolbox
  Needed for the `parfor` loops in ICA and power computation.
- Curve Fitting Toolbox
  Likely needed because [`functions/general/smooth_frequencies.m`](/functions/general/smooth_frequencies.m) uses `smooth`.



## Suggested MATLAB Setup

In practice, the simplest setup is:

1. Put the data on the drive layout expected by the path blocks, or edit the path-selection logic in [`Analysis/analysisParameters.m`](/Analysis/analysisParameters.m) and [`Preprocessing/prepParameters.m`](/Preprocessing/prepParameters.m).
2. Add `Matcycle`, `chART`, `fooof_mat`, EEGLAB, and optionally FieldTrip to the MATLAB path.
3. Start EEGLAB once so its plugins are registered.
4. Run preprocessing before analysis.


## Running The Pipeline

### Preprocessing

Run these in order:

1. [`Preprocessing/Prep1_Convert2MAT.m`](/Preprocessing/Prep1_Convert2MAT.m)
   Imports raw EGI or BrainVision recordings into EEGLAB `EEG` structs.
2. [`Preprocessing/Prep2_Filter.m`](/Preprocessing/Prep2_Filter.m)
   Builds filtered/downsampled copies for `ICA`, `Power`, and `Cutting`.
3. [`Preprocessing/Prep3_GetICA.m`](/Preprocessing/Prep3_GetICA.m)
   Detects bad channels and windows, runs ICA, and stores ICLabel outputs.
4. [`Preprocessing/Prep4_RemoveICA.m`](/Preprocessing/Prep4_RemoveICA.m)
   Removes artifactual components and performs final channel/window cleaning.
5. [`Preprocessing/Assemble_Metadata.m`](/Preprocessing/Assemble_Metadata.m)
   Builds `Metadata_Children_Wake.csv`.

Optional:

- [`Preprocessing/Prep4_ManuallyRemoveICA.m`](/Preprocessing/Prep4_ManuallyRemoveICA.m)
  Manual ICA review helper with hard-coded source and destination paths.

### Analysis

Run these in order:

1. [`Analysis/Analysis1_ComputePower.m`](/Analysis/Analysis1_ComputePower.m)
2. [`Analysis/Analysis2_DetectBursts.m`](/Analysis/Analysis2_DetectBursts.m)
3. [`Analysis/Analysis3_AssembleData.m`](/Analysis/Analysis3_AssembleData.m)
4. [`Analysis/PolishData.m`](/Analysis/PolishData.m)

Then run the figure/statistics scripts as needed:

- [`Analysis/Figure1_AnalysisGeneology.m`](/Analysis/Figure1_AnalysisGeneology.m)
- [`Analysis/Figure2_Specta.m`](/Analysis/Figure2_Specta.m)
- [`Analysis/Figure3_BasicMixedEffects.m`](/Analysis/Figure3_BasicMixedEffects.m)
- [`Analysis/Figure4_Figure6_TopographyAverage.m`](/Analysis/Figure4_Figure6_TopographyAverage.m)
- [`Analysis/Figure5_OvernightTopographies.m`](/Analysis/Figure5_OvernightTopographies.m)
- [`Analysis/Figure7_OvernightTopographiesBands.m`](/Analysis/Figure7_OvernightTopographiesBands.m)
- [`Analysis/Figure8_ADHD.m`](/Analysis/Figure8_ADHD.m)

## Mapping To The Paper

The core manuscript-ready exports are created in [`Analysis/PolishData.m`](/Analysis/PolishData.m):

- `ExtendedData_2.csv`
  Cleaned task-level metadata table.
- `ExtendedData_2.mat`
  `Metadata`, `SpectraAverage`, `Frequencies`.
- `ExtendedData_4.mat`
  `Metadata`, `Topographies`, `Chanlocs`.
- `ExtendedData_6.mat`
  `Metadata`, `TopographiesBands`, `Chanlocs`, `Bands`.

Figure scripts still mostly expect the full cached `ProcessedData.mat`. To make the exported Extended Data easier to use without editing the figure scripts, see [`Analysis/ExtendedData_Loaders.m`](/Analysis/ExtendedData_Loaders.m).
