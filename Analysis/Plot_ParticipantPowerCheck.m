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



%%% gather data
if Refresh || ~exist(fullfile(CacheDir, CacheName), 'file')
    EEGPowerSource = fullfile(Paths.AnalyzedData, 'EEG', 'Power', Folder);
    PowerSpectra = nan(Parameters.Participants, 4, 2, nFreqs);
    GammaTopographies = nan(Parameters.Participants, nChans);

    for DatasetCell = Datasets
        Dataset = DatasetCell{1};

        Tasks = list_filenames(fullfile(EEGPowerSource, Dataset))';
        Tasks(contains(Tasks, '.')) = [];

        for TaskIdx = 1:numel(Tasks)
            Task = Tasks{TaskIdx};

            % set paths and files
            TaskFiles = list_filenames(fullfile(EEGPowerSource, Dataset, Task));
            TaskFiles(~contains(TaskFiles, '.m')) = [];

            for TaskFile = TaskFiles'

                % get coordinates in mega matrix to save data
                Filename = TaskFile{1};
                Levels = split(Filename, '_');
                ParticipantID = str2double(Levels{1}(2:end));
                HourID = strcmp(Levels{4}, 'mor') + 1; % hack; 0+1 for evening, 1+1 for morning

                % load and save data
                load(fullfile(EEGPowerSource, Dataset, Task, Filename), 'Power', 'Chanlocs', 'Freqs')

                PrevSession = squeeze(PowerSpectra(ParticipantID, TaskIdx, HourID, :))';
                AllSessions = cat(1, PrevSession, mean(Power, 1, 'omitnan'));
                Spectrum = mean(AllSessions, 1, 'omitnan');
                SpectrumSmooth =  smooth_frequencies(Spectrum, Freqs, 2);
                PowerSpectra(ParticipantID, TaskIdx, HourID, :) = SpectrumSmooth;


                % gather topography of peak frequency
                if TaskIdx == 1 && HourID == 1
                    PeakFreq = find_periodic_peak(SpectrumSmooth, Freqs, GammaRange);
                    if ~isempty(PeakFreq)
                        BumpIndex = ismember(Freqs, PeakFreq);
                        GammaTopographies(ParticipantID, :) = Power(:, BumpIndex);
                    end
                end


                disp(['Finished ' Filename])
            end
        end
    end
    save(fullfile(CacheDir, CacheName), 'PowerSpectra', 'Freqs', 'Chanlocs')
else
    load(fullfile(CacheDir, CacheName), 'PowerSpectra', 'Freqs', 'Chanlocs')
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
        chART.save_figure([FigLabel,num2str(ParticipantIdx)], ResultsFolder, PlotProps)
        Idx = 1;
        figure('Units','normalized','OuterPosition',[0 0 1 1])
    elseif ParticipantIdx == Parameters.Participants
        chART.save_figure([FigLabel, num2str(ParticipantIdx)], ResultsFolder, PlotProps)
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







