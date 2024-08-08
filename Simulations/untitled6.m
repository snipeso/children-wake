% 
% WelchWindowLength = 4;
% Duration = numel(NREM)/SampleRate;
% [Power, FreqsOld] = cycy.utils.compute_power(NREM, SampleRate, WelchWindowLength, .5);
% PowerSmoothOld = cycy.utils.smooth_spectrum(Power, FreqsOld, SmoothSpan);
% FooofModel = fooof(FreqsOld, PowerSmoothOld, [1 40], struct(), true);
% 
% Intercept = FooofModel.aperiodic_params(1);
% disp(['Intercept ', num2str(WelchWindowLength), ' s welch window: ', num2str(Intercept)])
% 
% plot(FreqsOld, PowerSmoothOld)
% 
% 



WelchWindowLength = 5;

Durations = [1:50:1000];

Intercepts = nan(2, numel(Durations));
NREMSnippet = NREM(1:200*SampleRate);
for DurIdx = 1:numel(Durations)

Data = repmat(NREMSnippet, 1, Durations(DurIdx));

    [Power, FreqsOld] = cycy.utils.compute_power(Data, SampleRate, WelchWindowLength, .5);
PowerSmoothOld = cycy.utils.smooth_spectrum(Power, FreqsOld, SmoothSpan);
FooofModel = fooof(FreqsOld, PowerSmoothOld, [5 40], struct(), true);

Intercepts(1, DurIdx) = FooofModel.aperiodic_params(1);


[Power, FreqsOld] = cycy.utils.compute_power_fft(NREM(1:Durations(DurIdx)*SampleRate), SampleRate);
PowerSmoothOld = cycy.utils.smooth_spectrum(Power, FreqsOld, SmoothSpan);
FooofModel = fooof(FreqsOld, PowerSmoothOld, [5 40], struct(), true);

Intercepts(2, DurIdx) = FooofModel.aperiodic_params(1);
end

%%
figure
subplot(1, 2, 1)
scatter(Durations, Intercepts(1, :))
title('welch')

subplot(1,2,2)
% scatter(Durations, Intercepts(2, :))
scatter(Intercepts(1, end) - log10(Durations), Intercepts(2, :))
title('fft')