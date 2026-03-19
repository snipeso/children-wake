# Wake Oscillations In Children

This repository contains the MATLAB preprocessing, analysis, and figure code for the wake EEG paper in `Manuscript_v11.docx`. The code pools several child, adolescent, and adult datasets recorded in the evening before sleep and the morning after sleep, then quantifies oscillation amplitude, oscillation density, spectral power, and aperiodic measures.

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
  Per-recording spectral output from [`Analysis/Analysis1_ComputePower.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis1_ComputePower.m).
- `Bursts`, `BurstClusters`, `EEGMetadata`:
  Per-recording burst detections from [`Analysis/Analysis2_DetectBursts.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis2_DetectBursts.m).
- `Metadata`:
  Task-level summary table assembled in [`Analysis/Analysis3_AssembleData.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis3_AssembleData.m).
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

## Repository Layout

- [`Preprocessing`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing):
  Raw import, filtering, ICA, and metadata assembly.
- [`Analysis`](/mnt/c/Users/Sophia/Code/children-wake/Analysis):
  Power, burst, statistics, figure, and export scripts.
- [`functions/general`](/mnt/c/Users/Sophia/Code/children-wake/functions/general):
  Metadata and file-loading helpers.
- [`functions/eeg`](/mnt/c/Users/Sophia/Code/children-wake/functions/eeg):
  EEG cleaning, FOOOF/specparam wrappers, burst utilities, and channel helpers.
- [`functions/plots`](/mnt/c/Users/Sophia/Code/children-wake/functions/plots):
  Plotting helpers used by the figure scripts.
- [`functions/stats`](/mnt/c/Users/Sophia/Code/children-wake/functions/stats):
  Demographic summaries, t-tests, model reporting, and FDR helpers.
- [`functions/EGI`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI):
  Import code for EGI and BrainVision recordings.
- [`functions/external`](/mnt/c/Users/Sophia/Code/children-wake/functions/external):
  Vendored third-party MATLAB utilities.

## Required Repositories And Toolboxes

The analysis code depends on sibling repositories and MATLAB toolboxes that are not fully contained here.

### Sibling repositories

Keep these next to this repo, as in the current workspace:

- `../Matcycle`
  Provides the `cycy.*` functions used for Welch power and burst detection.
- `../chART`
  Provides plotting defaults and figure helpers.
- `../External/eeglab2025.1.0`
  Local EEGLAB checkout in this workspace.
- `../children-wake-python/fooof-wrapper/fooof_mat`
  Local MATLAB `fooof()` implementation visible in this workspace.

The original code also hard-codes older local or network paths in [`Analysis/analysisParameters.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/analysisParameters.m) and [`Preprocessing/prepParameters.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/prepParameters.m). If your directory layout differs, adapt those path blocks before running the pipeline.

### MATLAB toolboxes and external packages

Needed or strongly implied by the code:

- EEGLAB
  Required throughout preprocessing and topographic plotting.
- EEGLAB plugins:
  `ICLabel`, `clean_rawdata`, and BrainVision import support if you use `.vhdr/.eeg` files.
- FieldTrip
  Used in [`functions/EGI/load_eeg_data.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/EGI/load_eeg_data.m) to read trigger events from EGI raw files.
- `fooof_mat` / specparam MATLAB wrapper
  Required because the analysis calls `fooof()` directly.
- Statistics and Machine Learning Toolbox
  Needed for `fitlme`, `fitlm`, correlations, and t-tests.
- Signal Processing Toolbox
  Needed for `pwelch`, `designfilt`, `filtfilt`, and related filtering.
- Parallel Computing Toolbox
  Needed for the `parfor` loops in ICA and power computation.
- Curve Fitting Toolbox
  Likely needed because [`functions/general/smooth_frequencies.m`](/mnt/c/Users/Sophia/Code/children-wake/functions/general/smooth_frequencies.m) uses `smooth`.

## Suggested MATLAB Setup

In practice, the simplest setup is:

1. Put the data on the drive layout expected by the path blocks, or edit the path-selection logic in [`Analysis/analysisParameters.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/analysisParameters.m) and [`Preprocessing/prepParameters.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/prepParameters.m).
2. Add `Matcycle`, `chART`, `fooof_mat`, EEGLAB, and optionally FieldTrip to the MATLAB path.
3. Start EEGLAB once so its plugins are registered.
4. Run preprocessing before analysis.

If you want a local path layout similar to this workspace, the important folders are:

- `/mnt/c/Users/Sophia/Code/children-wake`
- `/mnt/c/Users/Sophia/Code/Matcycle`
- `/mnt/c/Users/Sophia/Code/chART`
- `/mnt/c/Users/Sophia/Code/External/eeglab2025.1.0`
- `/mnt/c/Users/Sophia/Code/children-wake-python/fooof-wrapper/fooof_mat`

## Expected Data Layout

The code expects an external data root such as `D:\Data\AllWake` or `/Volumes/Animalia/AllWake`, with subfolders like:

- `Raw`
- `Preprocessed`
- `Final`
- `Cache`
- `Results`
- `Metadata`
- `Errors`

Important metadata files expected on the data drive include:

- `Metadata/ParticipantCodes.csv`
- `Metadata/Participants_<Dataset>.csv`
- `Metadata/Data_<Dataset>.csv`
- `Metadata/Metadata_Children_Wake.csv`
- `Metadata/SleepScoring.mat`

Those data files are not included in this repository.

## Running The Pipeline

### Preprocessing

Run these in order:

1. [`Preprocessing/Prep1_Convert2MAT.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep1_Convert2MAT.m)
   Imports raw EGI or BrainVision recordings into EEGLAB `EEG` structs.
2. [`Preprocessing/Prep2_Filter.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep2_Filter.m)
   Builds filtered/downsampled copies for `ICA`, `Power`, and `Cutting`.
3. [`Preprocessing/Prep3_GetICA.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep3_GetICA.m)
   Detects bad channels and windows, runs ICA, and stores ICLabel outputs.
4. [`Preprocessing/Prep4_RemoveICA.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep4_RemoveICA.m)
   Removes artifactual components and performs final channel/window cleaning.
5. [`Preprocessing/Assemble_Metadata.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Assemble_Metadata.m)
   Builds `Metadata_Children_Wake.csv`.

Optional:

- [`Preprocessing/Prep4_ManuallyRemoveICA.m`](/mnt/c/Users/Sophia/Code/children-wake/Preprocessing/Prep4_ManuallyRemoveICA.m)
  Manual ICA review helper with hard-coded source and destination paths.

### Analysis

Run these in order:

1. [`Analysis/Analysis1_ComputePower.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis1_ComputePower.m)
2. [`Analysis/Analysis2_DetectBursts.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis2_DetectBursts.m)
3. [`Analysis/Analysis3_AssembleData.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Analysis3_AssembleData.m)
4. [`Analysis/PolishData.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/PolishData.m)

Then run the figure/statistics scripts as needed:

- [`Analysis/Figure1_AnalysisGeneology.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure1_AnalysisGeneology.m)
- [`Analysis/Figure2_Specta.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure2_Specta.m)
- [`Analysis/Figure3_BasicMixedEffects.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure3_BasicMixedEffects.m)
- [`Analysis/Figure4_Figure6_TopographyAverage.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure4_Figure6_TopographyAverage.m)
- [`Analysis/Figure5_OvernightTopographies.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure5_OvernightTopographies.m)
- [`Analysis/Figure7_OvernightTopographiesBands.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure7_OvernightTopographiesBands.m)
- [`Analysis/Figure8_ADHD.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/Figure8_ADHD.m)

## Mapping To The Paper

The core manuscript-ready exports are created in [`Analysis/PolishData.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/PolishData.m):

- `ExtendedData_2.csv`
  Cleaned task-level metadata table.
- `ExtendedData_2.mat`
  `Metadata`, `SpectraAverage`, `Frequencies`.
- `ExtendedData_4.mat`
  `Metadata`, `Topographies`, `Chanlocs`.
- `ExtendedData_6.mat`
  `Metadata`, `TopographiesBands`, `Chanlocs`, `Bands`.

Figure scripts still mostly expect the full cached `ProcessedData.mat`. To make the exported Extended Data easier to use without editing the figure scripts, see [`Analysis/ExtendedData_Loaders.m`](/mnt/c/Users/Sophia/Code/children-wake/Analysis/ExtendedData_Loaders.m).

## Notes And Caveats

- The preprocessing and analysis paths are machine-specific by design.
- Not all manuscript claims can be re-verified from this repository alone, because participant metadata and the actual EEG files live on the external data drive.
- The local workspace contains EEGLAB 2025.1.0, while the manuscript and code comments refer to earlier EEGLAB installs. Re-running the pipeline with a different EEGLAB version may slightly change outputs.
- The figure scripts are faithful to the original working environment, not optimized for turnkey reuse on a stripped-down shared dataset.

For script-by-script notes, see [`SCRIPT_COMMENTS.md`](/mnt/c/Users/Sophia/Code/children-wake/SCRIPT_COMMENTS.md). For possible paper/code mismatches, see [`MANUSCRIPT_POTENTIAL_DIFFERENCES.md`](/mnt/c/Users/Sophia/Code/children-wake/MANUSCRIPT_POTENTIAL_DIFFERENCES.md).
