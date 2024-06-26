# Wake Oscillations in Children

This project will analyze data from children 4-26, recorded before and after a night of sleep, to determine how oscillations change following sleep.

Research questions:
1. Do oscillation amplitudes decrease overnight, and does the decrease decrease with age, thus reflecting sleep homeostasis?
2. Is there an effect on ADHD?


## Datasets
###


## Pipeline

### Preprocessing

Preprequisites:
- fieldtrip (https://download.fieldtriptoolbox.org/) for extracting events in the EEG

Steps:

1. Prep1_Raw2MAT.m: extract data from EGI format into EEGLAB struct, saved to MAT files
2. Prep2_Filter.m: filter, downsample data. The core preprocessing function is [filter_and_downsample_eeg()](./functions/eeg/filter_and_downsample_eeg.m).
3. Prep3_GetICA.m: Remove bad channels and timepoints, run ICA. Removes slow! 
4. Prep4_RemoveICA.m: Remove artifact components based on 1/f slopes


### Analysis




### random codes
1 med in past
2 unmed
3 med day before
4 med day of
5 HC