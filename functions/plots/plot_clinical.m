function plot_clinical(EEG)

EEG = pop_reref(EEG, labels2indexes([57 100], EEG.chanlocs));

EEG = pop_select(EEG, 'channel', labels2indexes([22 9 24 124 36 104 52 92 70 83], EEG.chanlocs));

Labels = {'Fp1', 'Fp2', 'F3', 'F4', 'C3', 'C4', 'P3', 'P4', 'O1', 'O2'};

for Indx_L = 1:numel(Labels)
EEG.chanlocs(Indx_L).labels = Labels{Indx_L};
end

Pix = get(0,'screensize');
eegplot(EEG.data, 'srate', EEG.srate,  'spacing', 100, 'winlength', 10, ...
    'position', [0 0 Pix(3) Pix(4)*.97], 'events', EEG.event, 'eloc_file', EEG.chanlocs);