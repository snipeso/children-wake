function EEG = kispi_interpolate_bad_channels(EEG, artndxn)
% adapted from Maria Dimitriades.

ch=1:size(EEG.data, 1);

ndxgoodch=find(sum(artndxn')>0); %Indices of good channels
nchgood=length(ndxgoodch);  %Number of good chanels
ndxbadch=~ismember(ch,ndxgoodch); %Indices of bad channels
badch = find(ndxbadch); % bad channels
outerring=[43 48 49 56 63 68 73 81 88 94 99 107 113 119 120 125 126 127 128]; %outer ring; these will be excluded no matter what

badch(badch==129) = []; % discard any attempt to interpolate the empty Cz channel


badchouterring = sum(ismember(outerring, badch)); %bad channels to be interpolated that are in the outer ring
truebad = length(badch) - badchouterring;

if truebad >= 11
    fprintf('11 or more channels are bad! Setting bad channels to 0 and not interpolating') % Make sure 10% or less are not bad channels
    EEG.data(badch, :) = 0;
    return
else
    fprintf('Less than 11 bad channels!')
end

if nchgood ~= 128
    fprintf('Interpolating bad channels: %d %d %d %d %d %d %d %d %d \n', badch)
    EEG = eeg_interp(EEG, badch);
else
    fprintf('All channels are good')
end