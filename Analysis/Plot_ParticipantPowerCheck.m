% plot smooth average power spectrum for each participant

clear
clc
close all


Parameters = analysisParameters();
Paths = Parameters.Paths;
Datasets = Parameters.Datasets;
Refresh = true;
PlotProps = Parameters.PlotProps.Manuscript;

nFreqs = 1025;
nChans = 109;
Folder = 'window8s_allt/';

CacheDir = Paths.Cache;
CacheName = 'AllAveragePower.m';

if ~exist(CacheDir, 'dir')
    mkdir(CacheDir)
end

ResultsFolder = fullfile(Paths.Results, 'PowerSpectra');
if ~exist(ResultsFolder,'dir')
    mkdir(ResultsFolder)
end

GammaRange = [25, 35];

Metadata = readtable(fullfile(Paths.Metadata, 'Metadata.csv'));
Metadata = Metadata(contains(Metadata.Dataset, Datasets), :);

[Participants, UniqueIndx] = unique(Metadata.Participant);
UniqueMetadata = Metadata(UniqueIndx, :);

Hours = Parameters.Hours;

%%% gather data
if Refresh || ~exist(fullfile(CacheDir, CacheName), 'file')
    EEGPowerSource = fullfile(Paths.AnalyzedData, 'EEG', 'Power', Folder);
    PowerSpectra = nan(numel(Participants), 4, 4, 2, nFreqs); % P x S x T x H x F
    GammaTopographies = nan(numel(Participants), nChans);

    for ParticipantIdx = 1:numel(Participants)
        Participant = Participants{ParticipantIdx};
        Dataset = UniqueMetadata.Dataset{strcmp(UniqueMetadata.Participant, Participant)};
        Tasks = Parameters.Tasks.(Dataset);
        Sessions = Parameters.Sessions.(Dataset);

        for TaskIdx = 1:numel(Tasks)
            Task = Tasks{TaskIdx};
            for SessionIdx = 1:numel(Sessions)
                Session = Sessions{SessionIdx};
                for HourIdx = 1:numel(Hours)
                    Hour = Hours{HourIdx};

                    % load in data
                    Path = fullfile(Paths.AnalyzedData, 'EEG', 'Power', Folder, Dataset, Task);
                    Power = load_datafile(Path, Participant, Session, Hour, 'Power', '.mat');
                    if isempty(Power); continue; end
                    if ~exist('Chanlocs', 'var')
                        Chanlocs = load_datafile(Path, Participant, Session, Hour, 'Chanlocs', '.mat');
                        Freqs = load_datafile(Path, Participant, Session, Hour, 'Freqs', '.mat');
                    end

                    % average across channels
                    Spectrum = mean(Power, 1, 'omitnan');
                    SpectrumSmooth =  smooth_frequencies(Spectrum, Freqs, 2);
                    PowerSpectra(ParticipantIdx, SessionIdx, TaskIdx, HourIdx, :) = SpectrumSmooth;


                    % gather topography of peak frequency
                    if TaskIdx == 1 && HourIdx == 1 && SessionIdx == 1
                        PeakFreq = find_periodic_peak(SpectrumSmooth, Freqs, GammaRange);
                        if ~isempty(PeakFreq)
                            BumpIndex = ismember(Freqs, PeakFreq);
                            GammaTopographies(ParticipantIdx, :) = Power(:, BumpIndex);
                        end
                    end
                end

            end
            disp(['Finished ' Participants{ParticipantIdx}])
        end
    end
    save(fullfile(CacheDir, CacheName), 'PowerSpectra', 'Freqs', 'Chanlocs', 'GammaTopographies')
else
    load(fullfile(CacheDir, CacheName), 'PowerSpectra', 'Freqs', 'Chanlocs', 'GammaTopographies')
end




%% plot spectra

close all

plotGamma = true;
FigureDimentions = [5 8];

Colors = chART.color_picker([4, 2]);

if plotGamma
    Range = [25 40];
    FigLabel = 'GAMMAPower_';
else
    Range = [1 40];
    FigLabel = 'AllPower_';
end
Idx = 1;
figure('Units','normalized','OuterPosition',[0 0 1 1])
for ParticipantIdx = 1:Parameters.Participants

    subplot(FigureDimentions(1), FigureDimentions(2), Idx)
    hold on
    for TaskIdx = 1:4
        for HourIdx = 1:2
            Data = squeeze(PowerSpectra(ParticipantIdx, TaskIdx, HourIdx, :));

            plot(Freqs, Data, 'LineWidth', 1.5, 'Color', squeeze(Colors(HourIdx, :, TaskIdx)));
            xlabel('Frequency (Hz)')
            ylabel('Power')
            set(gca, 'XScale', 'log', 'YScale', 'log', 'xlim', Range)
            title(ParticipantIdx)
        end
    end


    Idx = Idx+1;
    if Idx > FigureDimentions(1)*FigureDimentions(2)
        % chART.save_figure([FigLabel,num2str(ParticipantIdx)], ResultsFolder, PlotProps)
        Idx = 1;
        figure('Units','normalized','OuterPosition',[0 0 1 1])
    elseif ParticipantIdx == Parameters.Participants
        % chART.save_figure([FigLabel, num2str(ParticipantIdx)], ResultsFolder, PlotProps)
    end
end


%% plot gamma topographies


FigLabel = 'GammaTopo';
Idx = 1;
figure('Units','normalized','OuterPosition',[0 0 1 1])
CLims = 'minmax';
for ParticipantIdx = 1:Parameters.Participants

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







