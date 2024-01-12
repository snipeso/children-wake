function plot_quick_eeg(EEG, PlotICA)
arguments
    EEG
    PlotICA = false;
end

Pix = get(0,'screensize');
if PlotICA
    tmpdata = eeg_getdatact(EEG, 'component', 1:nComps);
    DispChannels = 40;
    Spacing = 5;
else
    tmpdata = EEG.data;
    DispChannels = size(tmpdata, 1);
    Spacing = 20;
end
eegplot( tmpdata, 'srate', EEG.srate,  'spacing', Spacing, 'dispchans', DispChannels, ...
    'winlength', 20, 'position', [0 0 Pix(3) Pix(4)*.97], 'events', EEG.event, 'eloc_file', EEG.chanlocs);