function plotComps(EEG)



% open interface for selecting components
StandardColor = {[0.19608  0.19608  0.51765]}; % for plotting

NoiseColors = [
    112, 173, 71; % brain
    237, 125, 49; % muscle
    255, 192, 0; % eyes
    239, 67, 169; % heart
    10, 10, 150; % line noise
    200, 200, 200; % channel noise
    50, 50, 50; % other 
    ]/255;

Pix = get(0,'screensize');

% turn red all the bad components
nComps = size(EEG.icaweights,1);
Colors = repmat(StandardColor, nComps, 1);

% plot differently for each component type
Manual = EEG.manual;

% for Indx_T = 1:numel(Manual)
%     Colors(Manual==Indx_T) = {NoiseColors(Indx_T, :)};
% end

% plot in time all the components
tmpdata = eeg_getdatact(EEG, 'component', 1:nComps);
tmpdata = tmpdata-median(tmpdata, 2);
eegplot(tmpdata, 'srate', EEG.srate,  'spacing', 5, ...
    'winlength', 20, 'position', [0 0 Pix(3) Pix(4)*.97], ...
    'color', Colors, 'limits', [EEG.xmin EEG.xmax]*1000);
end