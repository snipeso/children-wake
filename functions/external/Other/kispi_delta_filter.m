function EEG = kispi_delta_filter(EEG)
% filters sleep data with filters that are fairly fast, and are good for
% slow wave detection. Provided by Maria Dimitriades


% filter to remove extra strong line noise
srateFilt    = EEG.srate;
PassFrq      = 30;
StopFrq      = 48; % NB: this code breaks for uneven filter orders; I changed this value to make sure that didn't happen since I use a different sample rate from everyone else
PassRipple   = 0.02;
StopAtten    = 60;
LoPassFilt   = designfilt('lowpassfir','PassbandFrequency',PassFrq,'StopbandFrequency',StopFrq,'PassbandRipple',PassRipple,'StopbandAttenuation',StopAtten, 'SampleRate',srateFilt, 'DesignMethod','kaiser');
EEG  = firfilt(EEG, LoPassFilt.Coefficients);



%  %% Filter the data in the slow wave range data

% filter parameters
passbanddeltat1 = 0.5;
passbanddeltat2 = 4;
stopbanddeltat1 = 0.1;
stopbanddeltat2 = 10;
Fs = EEG.srate; % Sampling frequency in Hz
Wp = [passbanddeltat1 passbanddeltat2] / (Fs / 2); % Passband normalized to Nyquist frequency
Ws = [stopbanddeltat1 stopbanddeltat2] / (Fs / 2); % Stopband normalized to Nyquist frequency
Rp = 3; % Passband ripple in dB
Rs = 10; % Stopband attenuation in dB

% Calculate the filter order and coefficients
[n, Wn] = cheb2ord(Wp, Ws, Rp, Rs);
[b_cheby2, a_cheby2] = cheby2(n, Rs, Wn);

% Apply the filter to each channel
for channel = 1:size(EEG.data, 1)
    EEG.data(channel, :) = filtfilt(b_cheby2, a_cheby2, double(EEG.data(channel, :)));
    disp(['finished ch', num2str(channel)])
end

