clear
clc
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameters

Parameters = simulationParameters();
Paths = Parameters.Paths;
ResultsFolder = Paths.Results;

WelchWindow = 4;
WelchWindowOverlap = .5;

% sim parameters
Duration = 60*6; % the data's average
SampleRate = 250;

nPoints = 10;

AllMeasures = struct();
AllMeasures.Amplitudes = linspace(0, 50, nPoints);
AllMeasures.Densities = linspace(0, 1, nPoints);
% AllMeasures.Exponents = linspace(0, 4, nPoints);
AllMeasures.Exponents = linspace(1.1, 2.5, nPoints);
AllMeasures.Offsets = linspace(.1, 3, nPoints);
BurstFrequency = 10;
BurstDuration = 1;

ProtoMeasures = struct();
ProtoMeasures.Amplitudes = 30;
ProtoMeasures.Densities = .2;
ProtoMeasures.Exponents = 1.5;
ProtoMeasures.Offsets = 1.5;

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

OutcomeLabels = ['nBursts'; MeasureLabels; 'Power'; 'PeriodicPower'];
nOutcomes = numel(OutcomeLabels);
Outcomes = struct();
for OutcomeIdx = 1:nOutcomes
    for MeasureIdx = 1:nMeasures
        Outcomes.(MeasureLabels{MeasureIdx}).(OutcomeLabels{OutcomeIdx}) = nan(nPoints, 1);
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
    for Idx = 1:nPoints

        % run simulation
        Measures.(Mes) = X(Idx);
        
        [Aperiodic, t] = cycy.sim.simulate_aperiodic_eeg(-Measures.Exponents, Measures.Offsets, Duration, SampleRate, WelchWindow);

        fAperiodic = cycy.utils.highpass_filter(Aperiodic, SampleRate, 0.8, 0.4, 'equiripple', 1, 80);
        fAperiodic = cycy.utils.lowpass_filter(fAperiodic, SampleRate, 50, 55);

        [Periodic, ~] = cycy.sim.simulate_periodic_eeg(BurstFrequency, Measures.Amplitudes, Measures.Densities, BurstDuration, Duration, SampleRate);

        sumData = fAperiodic + Periodic;

        % calculate new power spectrum
        [Power, Freqs] = cycy.utils.compute_power(sumData, SampleRate, WelchWindow, WelchWindowOverlap);
        PowerSmooth = cycy.utils.smooth_spectrum(Power, Freqs, SmoothSpan);

        % plot spectrum
        plot(Freqs, PowerSmooth, 'Color', Colors(Idx, :), 'LineWidth',1.5)
        chART.set_axis_properties(PlotProps)

        % run FOOOF
        [Exponent, Offset, PeriodicPower, FooofFrequencies] = fooof_spectrum(Power, Freqs, [2 35]);

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

        Outcomes.(Mes).nBursts(Idx) = numel(Bursts);
        if isempty(Bursts) || numel(Bursts)<10
            continue
        end
        Outcomes.(Mes).nBursts(Idx) = numel(Bursts);
        Outcomes.(Mes).Amplitudes(Idx) = mean([Bursts.Amplitude]);
        Outcomes.(Mes).Densities(Idx) = sum([Bursts.DurationPoints])/numel(sumData);

    end
    set(gca, 'YScale', 'log', 'XScale', 'log');
    title(Mes)
    xlabel('Frequency (Hz)')
    ylabel('Power')
    xlim([3 50])

    ylim([10^-4 10^2])
    chART.save_figure(['SimSpectrum_' Mes, '.svg'], ResultsFolder, PlotProps)
    disp(['finished ', Mes])
end




%% plot all

% get figure limits

AllTable = table();
for MeasureIdx = 1:nMeasures

    T = struct2table(Outcomes.(MeasureLabels{MeasureIdx}));

    T.(MeasureLabels{MeasureIdx}) = nan(nPoints, 1);

    AllTable = [AllTable; T];
end



PlotProps = Parameters.PlotProps.Manuscript;
Grid = [nOutcomes, nMeasures];
% PlotProps.Figure.Padding = 50;
PlotProps.Axes.xPadding = 2;
PlotProps.Axes.yPadding = 2;


YLims = [min(AllTable{:, :})', max(AllTable{:, :})'];

figure('Units','centimeters', 'Position',[0 0 20 25])

for MeasureIdx = 1:nMeasures
    Mes = MeasureLabels{MeasureIdx};

    for OutcomeIdx = 1:nOutcomes
        X = AllMeasures.(Mes);
        Y = Outcomes.(Mes).(OutcomeLabels{OutcomeIdx});

        Color = Colors(1:numel(X), :);

        if strcmp(Mes, OutcomeLabels{OutcomeIdx})
            Color = chART.utils.pale_colors(Color, .3);
        end

        chART.sub_plot([], Grid, [OutcomeIdx, MeasureIdx], [], true, '', PlotProps);
        hold on
        plot([ProtoMeasures.(Mes), ProtoMeasures.(Mes)], YLims(OutcomeIdx, :), 'Color', [.8 .8 .8])
        scatter(X, Y, 30, Color, "filled")
        ylim(YLims(OutcomeIdx, :))
        xlim([X(1), X(end)])
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

chART.save_figure(['SimInteractions.svg'], ResultsFolder, PlotProps)







