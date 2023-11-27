% quick check adults
clear
clc
close all

Parameters = analysisParameters();
Paths = Parameters.Paths;
PlotProps = Parameters.PlotProps.Manuscript;
Path = 'E:\Data\Final\EEG\Unlocked\window8s_duration4m';
ResultsFolder = fullfile(Paths.Results, 'PowerSpectra');

Tasks = {'Game', 'Music', 'SpFT'};
Sessions = {'Session2','Baseline'};

% Tasks = {'Fixation', 'Standing', 'Oddball'};
% Sessions = {'Main7', 'BaselinePost'};
Participants = {'P01', 'P02', 'P03', 'P04', 'P05', 'P06', 'P07', 'P08', ...
    'P09', 'P10', 'P11', 'P12', 'P13', 'P14', 'P15', 'P16', 'P17', 'P19'};

GammaRange = [30, 40];
nFreqs = 1025;
nChans = 120;


PowerSpectra = nan(numel(Participants), numel(Tasks), numel(Sessions), nFreqs);
GammaTopographies = nan(Parameters.Participants, nChans);

for TaskIdx = 1:numel(Tasks)
    Task = Tasks{TaskIdx};
    for ParticipantIdx = 1:numel(Participants)
        for SessionIdx = 1:numel(Sessions)

            Filename = strjoin({Participants{ParticipantIdx}, Task, Sessions{SessionIdx}, 'Welch.mat'}, '_');

            Filepath = fullfile(Path, Task, Filename);
            if ~exist(Filepath, 'file')
                continue
            end
            load(Filepath, 'Power', 'Chanlocs', 'Freqs')
Spectrum = mean(Power, 1, 'omitnan');
            % Spectrum = mean(Power(labels2indexes(Parameters.Channels.PreROI.Front, Chanlocs), :), 1, 'omitnan');
            SpectrumSmooth =  smooth_frequencies(Spectrum, Freqs, 2);
            PowerSpectra(ParticipantIdx, TaskIdx, SessionIdx, :) = SpectrumSmooth;
            if TaskIdx == 1 && SessionIdx == numel(Sessions)
                PeakFreq = find_periodic_peak(SpectrumSmooth, Freqs, GammaRange);
                if ~isempty(PeakFreq)
                    BumpIndex = ismember(Freqs, PeakFreq);
                    GammaTopographies(ParticipantIdx, :) = Power(:, BumpIndex);
                end
            end
        end
    end
end

%%

% close all

plotGamma = tru e;
FigureDimentions = [4 5];

Colors = chART.color_picker([numel(Tasks), numel(Sessions)]);

if plotGamma
    Range = [25 40];
    FigLabel = 'AdultGAMMAPower';
else
    Range = [1 40];
    FigLabel = 'AdultAllPower';
end

figure('Units','normalized','OuterPosition',[0 0 1 1])
for ParticipantIdx = 1:numel(Participants)

    subplot(FigureDimentions(1), FigureDimentions(2), ParticipantIdx)
    hold on
    for TaskIdx = 1:numel(Tasks)
        for SessionIdx = 1:numel(Sessions)
            Data = squeeze(PowerSpectra(ParticipantIdx, TaskIdx, SessionIdx, :));

            plot(Freqs, Data, 'LineWidth', 1.5, 'Color', squeeze(Colors(SessionIdx, :, TaskIdx)));
            xlabel('Frequency (Hz)')
            ylabel('Power')
            set(gca, 'XScale', 'log', 'YScale', 'log', 'xlim', Range)
            title(ParticipantIdx)
        end
    end
end

 chART.save_figure(FigLabel, ResultsFolder, PlotProps)


 %% plot gamma topographies


FigLabel = 'AdultGammaTopo';
Idx = 1;
figure('Units','normalized','OuterPosition',[0 0 1 1])
CLims = 'minmax';
for ParticipantIdx = 1:numel(Participants)

    Data = GammaTopographies(ParticipantIdx, :);
    if ~all(isnan(Data))
        subplot(FigureDimentions(1), FigureDimentions(2), Idx)
        hold on
        chART.plot.eeglab_topoplot(Data, Chanlocs, [], CLims, '', 'Linear', PlotProps)
        colorbar off
        title(ParticipantIdx)
    end

    Idx = Idx+1;
    if Idx > FigureDimentions(1)*FigureDimentions(2)
        chART.save_figure([FigLabel,num2str(ParticipantIdx)], ResultsFolder, PlotProps)
        Idx = 1;
        figure('Units','normalized','OuterPosition',[0 0 1 1])
    elseif ParticipantIdx == Parameters.Participants
        chART.save_figure([FigLabel, num2str(ParticipantIdx)], ResultsFolder, PlotProps)
    end
end



