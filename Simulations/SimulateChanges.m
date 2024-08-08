clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameters

Parameters = simulationParameters();
Paths = Parameters.Paths;
ResultsFolder = Paths.Results;

% sim parameters
Duration = 60*6; % the data's average
SampleRate = 250;

AllMeasures = struct();
AllMeasures.Amplitudes = linspace(0, 100, 20);
AllMeasures.Densities = linspace(0, 1, 20);
AllMeasures.Exponents = linspace(0, 4, 20);
AllMeasures.Offsets = linspace(-2, 2, 20);
BurstFrequency = 10;
BurstDuration = 1;

ProtoMeasures = struct();
ProtoMeasures.Amplitudes = 20;
ProtoMeasures.Densities = .5;
ProtoMeasures.Exponents = 1.5;
ProtoMeasures.Offsets = -.5;

% analysis parameters
SmoothSpan = 2;
BurstRange = [8 12];
PowerRange = [4 16];

CriteriaSet = struct();
CriteriaSet.PeriodConsistency = .5;
CriteriaSet.AmplitudeConsistency = .4;
CriteriaSet.FlankConsistency = .5;
CriteriaSet.ShapeConsistency = .2;
CriteriaSet.MonotonicityInTime = .4;
CriteriaSet.MonotonicityInAmplitude = .4;
CriteriaSet.ReversalRatio = .6;
CriteriaSet.MinCyclesPerBurst = 4;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run

PlotProps = Parameters.PlotProps.Manuscript;

% setup outcome structure
MeasureLabels = fieldnames(AllMeasures);
nMeasures = numel(MeasureLabels);

OutcomeLabels = [MeasureLabels; 'Power'; 'PeriodicPower'];
nOutcomes = numel(OutcomeLabels);
Outcomes = struct();
for OutcomeIdx = 1:nOutcomes
    for MeasureIdx = 1:nMeasures
        Outcomes.(MeasureLabels{MeasureIdx}).(OutcomeLabels{OutcomeIdx}) = nan(size(AllMeasures.(MeasureLabels{MeasureIdx})));
    end
end

% run simulations
for MeasureIdx = 1:nMeasures

    Mes = MeasureLabels{MeasureIdx};
    X = AllMeasures.(Mes);

    Measures = ProtoMeasures;

    Colors = chART.utils.resize_colormap(PlotProps.Color.Maps.Linear, numel(X)+2);

    figure('Units','centimeters', 'Position', [0 0 10 10])
    hold on
    for Idx = 1:numel(X)

        % run simulation
        Measures.(Mes) = X(Idx);

        [Aperiodic, t] = cycy.sim.simulate_aperiodic_eeg(-Measures.Exponents, Measures.Offsets, Duration, SampleRate);

        fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
        fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 50, 55);

        [Periodic, ~] = cycy.sim.simulate_periodic_eeg(BurstFrequency, Measures.Amplitudes, Measures.Densities, BurstDuration, Duration, SampleRate);

        sumData = fAperiodic + Periodic;

        % calculate new power spectrum
        [Power, Freqs] = cycy.utils.compute_power_fft(sumData, SampleRate);
        PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);

        % plot spectrum
        plot(Freqs, PowerSmooth, 'Color', Colors(Idx, :), 'LineWidth',1.5)
        chART.set_axis_properties(PlotProps)

        % run FOOOF
        [Exponent, Offset, PeriodicPower, FooofFrequencies] = fooof_spectrum(Power, Freqs);

        % run cycle-by-cycle analysis
        DataNarrowband = cycy.utils.highpass_filter(sumData, SampleRate, BurstRange(1)); % if you want, you can specify other aspects of the filter; see function
        DataNarrowband = cycy.utils.lowpass_filter(DataNarrowband, SampleRate, BurstRange(2));

        Cycles = cycy.detect_cycles(sumData, DataNarrowband);
        AugmentedCycles = cycy.measure_cycle_properties(sumData, Cycles, SampleRate);
        [Bursts, Diagnostics] = cycy.aggregate_cycles_into_bursts(AugmentedCycles, CriteriaSet);

        % assign outcome measures
        Range = dsearchn(Freqs', PowerRange');
        Outcomes.(Mes).Power(Idx) = mean(log10(PowerSmooth(Range(1):Range(2))));

        Range = dsearchn(FooofFrequencies', PowerRange');
        Outcomes.(Mes).PeriodicPower(Idx) = mean(PeriodicPower(Range(1):Range(2)));

        Outcomes.(Mes).Exponents(Idx) = Exponent;
        Outcomes.(Mes).Offsets(Idx) = Offset;

        if isempty(Bursts)
            continue
        end

        Outcomes.(Mes).Amplitudes(Idx) = mean([Bursts.Amplitude]);
        Outcomes.(Mes).Densities(Idx) = sum([Bursts.DurationPoints])/numel(sumData);

    end
    set(gca, 'YScale', 'log', 'XScale', 'log');
    title(Mes)
    xlabel('Frequency (Hz)')
    ylabel('Power')
    xlim([3 50])

    % chART.save_figure(['SimSpectrum_' Mes, '.png'], ResultsFolder, PlotProps)
disp(['finished ', Mes])
end


%% all

PlotProps = Parameters.PlotProps.Manuscript;
Grid = [nOutcomes, nMeasures];
PlotProps.Axes.xPadding = 1;
PlotProps.Axes.yPadding = 1;


figure('Units','centimeters', 'Position',[0 0 20 25])

for MeasureIdx = 1:nMeasures
        Mes = MeasureLabels{MeasureIdx};

    for OutcomeIdx = 1:nOutcomes
        X = AllMeasures.(Mes);
        Y = Outcomes.(Mes).(OutcomeLabels{OutcomeIdx});

        chART.sub_plot([], Grid, [OutcomeIdx, MeasureIdx], [], true, '', PlotProps);
        scatter(X, Y, 30, Colors(1:numel(X), :), "filled")
        chART.set_axis_properties(PlotProps)
        if OutcomeIdx == nOutcomes
            xlabel(Mes)
        elseif OutcomeIdx ==1
            title(Mes)
        end

        if MeasureIdx == 1
            ylabel(OutcomeLabels{OutcomeIdx})
        end
    end
end








