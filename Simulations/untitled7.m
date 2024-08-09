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

nPointsX = 20;
nPointsC = 10;

CMeasures = struct();
XMeasures = struct();
CMeasures.Amplitude = linspace(0, 50, nPointsC);
XMeasures.Density = linspace(0, 1, nPointsX);
XMeasures.Exponent = linspace(0, 4, nPointsX);
CMeasures.Offset = linspace(.1, 3, nPointsC);

Proto = struct();
Proto.Amplitude = 20;
Proto.Density = .2;
Proto.Duration = 1;
Proto.Frequency = 10;

Proto.Exponent = 1.5;
Proto.Offset = 1.5;

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

XMeasureLabels = fieldnames(XMeasures);
CMeasureLabels = fieldnames(CMeasures);
OutcomeLabels = {'nBursts' 'Amplitudes', 'Densities', 'Exponents', 'Offsets', 'Power', 'PeriodicPower'};
nOutcomes = numel(OutcomeLabels);
Outcomes = nan(2, nOutcomes,  nPointsX, nPointsC);

% run simulations
for MeasureIdx = 1:numel(XMeasureLabels)

    MesX = XMeasureLabels{MeasureIdx};
    X = XMeasures.(MesX);

    EEGParameters = Proto;
    MesC = CMeasureLabels{MeasureIdx};
    C = CMeasures.(MesC);

    for IdxX = 1:nPointsX
        for IdxC = 1:nPointsC

            % run simulation
            EEGParameters.(MesX) = X(IdxX);
            EEGParameters.(MesC) = C(IdxC);

            [Signal, t] = cycy.sim.eeg(EEGParameters, EEGParameters, Duration, SampleRate, WelchWindow);


            [Power, Freqs, Outcomes(MeasureIdx, 4, IdxX, IdxC), ... % exponent
                Outcomes(MeasureIdx, 5, IdxX, IdxC), ... % offset
                PeriodicPower, FooofFrequencies,...
                Outcomes(MeasureIdx, 2, IdxX, IdxC), ... % burst amplitudes
                Outcomes(MeasureIdx, 3, IdxX, IdxC),... % densities
                Outcomes(MeasureIdx, 1, IdxX, IdxC)] = simulate_analysis(Signal); % nBursts
        end
    end
end


%% plot

PlotProps = Parameters.PlotProps.Manuscript;
PlotProps.Axes.xPadding = 10;

PlotProps.Axes.yPadding = 10;

Colors = chART.utils.resize_colormap(PlotProps.Color.Maps.Linear, nPointsC+2);

Grid = [2 nOutcomes];


figure('Units','normalized', 'OuterPosition',[0 0 1 1])

for MeasureIdx = 1:2
    for OutcomeIdx = 1:nOutcomes
        X = XMeasures.(XMeasureLabels{MeasureIdx});
        C = CMeasures.(CMeasureLabels{MeasureIdx});
        chART.sub_plot([], Grid, [MeasureIdx, OutcomeIdx], [], true, '', PlotProps);
        hold on
        for IdxC = 1:nPointsC

            plot(X, squeeze(Outcomes(MeasureIdx, OutcomeIdx, :, IdxC))', 'Color', Colors(IdxC, :), 'LineWidth',2)
            chART.set_axis_properties(PlotProps)
        end
        % title(XMeasureLabels{MeasureXIdx})
        xlabel(XMeasureLabels{MeasureIdx})
        title(OutcomeLabels{OutcomeIdx})
        legend(append(string(round(C, 1)), [' ',CMeasureLabels{MeasureIdx}]))
        set(legend, 'ItemTokenSize', [10 10])
    end
end



