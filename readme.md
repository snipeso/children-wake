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
2. Prep2_Filter.m: filter, downsample data
3. Prep3_GetICA.m: Remove bad channels and timepoints, run ICA. Removes slow! 
4. Prep4_RemoveICA.m: Remove artifact components based on 1/f slopes


### Analysis



# NB
Orginal repo was AllWake

## TODO
- run preprocessing (maybe rerun) on all data + adults
- Make sure I have all the function dependencies, especially from AllWake for preprocessing
- if time: run FOOOF, show change in slopes
- get Koffein metadata
- quality check: plot all participants and their sessions in one plot (see how differnt mor/eve can be), and use to exclude bad participants
- handedness of BMSSL
- finish loading in birthdates BMSSLL!!