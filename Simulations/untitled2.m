% Simulation parameters
fs = 250;        % Sampling frequency (Hz)
n_seconds = 60*1;    % Duration of the signal (seconds)
n_samples = fs * n_seconds;  % Total number of samples

% Generate time vector
times = (0:n_samples-1) / fs;

% Generate aperiodic (1/f) signal
exponent = 1.5;     % Exponent of the 1/f process
intercept = 1.5;
freqs = (0:(n_samples/2)) / n_seconds;
% spectrum = randn(size(freqs)) .* (1 ./ freqs).^(exponent / 2);
spectrum = intercept+randn(size(freqs)) .* (1 ./ freqs).^(exponent / 2);
% spectrum = (1 ./ freqs).^(exponent / 2);
spectrum(1) = 0;  % Set the DC component to zero

% Create the full spectrum (conjugate symmetric)
full_spectrum = [spectrum, fliplr(spectrum(2:end-1))];

figure
plot(freqs, spectrum)

% Convert the spectrum back to the time domain
aperiodic_signal = ifft(full_spectrum, 'symmetric');

% Plot the aperiodic signal
figure;
plot(times, aperiodic_signal, 'LineWidth', 1);
title('Simulated Aperiodic Signal');
xlabel('Time (s)');
ylabel('Amplitude');


 [Power, Frequencies] = oscip.compute_power(aperiodic_signal, fs, 4, .5);
 figure
 plot(Frequencies, Power)
 set(gca, 'XScale','log', 'YScale', 'log')