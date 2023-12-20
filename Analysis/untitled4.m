
T = linspace(0, 60, 60*200);
WholeSignal = sin(T*2*pi*10);
Signal = WholeSignal;

Signal(4000:end) = 0;

figure;plot(T, Signal)

[Power, Freqs] = cycy.utils.compute_power(Signal, 200);

figure;
% plot(Freqs, log(Power))
plot(Freqs, Power)
hold on

Signal2 = 3*Signal;
[Power, Freqs] = cycy.utils.compute_power(Signal2, 200);
plot(Freqs, Power)


Signal3 = WholeSignal;
% Signal3(8000:end) = 0;
[Power, Freqs] = cycy.utils.compute_power(Signal3, 200);
plot(Freqs, Power)

legend({'Normal', '3x Amplitude', '3x quantity'})