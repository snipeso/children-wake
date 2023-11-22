
% plot smooth average power spectrum for each participant

clear
clc
close all


Parameters = analysisParameters();
Paths = Parameters.Paths;
Dataset = 'SleepLearning';
Task = '1Oddball';
Refresh = true;
PlotProps = Parameters.PlotProps.Manuscript;

nFreqs = 1025;
nChans = 109;
Folder = 'window8s_allt/';

CacheDir = Paths.Cache;
CacheName = 'SLAveragePower.m';

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
    EEGPowerSource = fullfile(Paths.AnalyzedData, 'EEG', 'Power', Folder, Dataset, Task);
    Files = list_filenames(EEGPowerSource);
    Files(~contains(Files, '.m')) = [];
    AllLevels = split(Files, '_');
    Participants = unique(AllLevels(:, 1));
    Sessions = unique(AllLevels(:, 3));

    PowerSpectra = nan(numel(Participants), numel(Sessions), 2, nFreqs);
    GammaTopographies = nan(Parameters.Participants, numel(Sessions), nChans);

    for File = Files'

        % get coordinates in mega matrix to save data
        Filename = File{1};
        Levels = split(Filename, '_');
        ParticipantIdx = ismember(Participants, Levels(1));
        SessionIdx = ismember(Sessions, Levels(3));
        HourIdx = strcmp(Levels{4}, 'mor') + 1; % hack; 0+1 for evening, 1+1 for morning

        % load and save data
        load(fullfile(EEGPowerSource, Filename), 'Power', 'Chanlocs', 'Freqs')
        Spectrum = mean(Power, 1, 'omitnan');
        SpectrumSmooth = smooth_frequencies(Spectrum, Freqs, 2);
        PowerSpectra(ParticipantIdx, SessionIdx, HourIdx, :) = SpectrumSmooth;


        % gather topography of peak frequency
        if HourIdx == 1
            PeakFreq = find_periodic_peak(SpectrumSmooth, Freqs, GammaRange);
            if ~isempty(PeakFreq)
                BumpIndex = ismember(Freqs, PeakFreq);
                GammaTopographies(ParticipantIdx, SessionIdx, :) = Power(:, BumpIndex);
            end
        end
        disp(['Finished ' Filename])
    end
    save(fullfile(CacheDir, CacheName), 'PowerSpectra', 'Freqs', 'Chanlocs', 'GammaTopographies', 'Participants', 'Sessions')
else
    load(fullfile(CacheDir, CacheName), 'PowerSpectra', 'Freqs', 'Chanlocs', 'GammaTopographies', 'Participants', 'Sessions')
end




%% plot spectra

% close all

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
for ParticipantIdx = 1:numel(Participants)

    subplot(FigureDimentions(1), FigureDimentions(2), Idx)
    hold on
    for TaskIdx = 1:4
        for HourIdx = 1 %1:2
            Data = squeeze(PowerSpectra(ParticipantIdx, TaskIdx, HourIdx, :));

            plot(Freqs, Data, 'LineWidth', 1.5, 'Color', squeeze(Colors(HourIdx, :, TaskIdx)));
            xlabel('Frequency (Hz)')
            ylabel('Power')
            set(gca, 'XScale', 'log', 'YScale', 'log', 'xlim', Range)
            title(Participants(ParticipantIdx))
        end
    end


    Idx = Idx+1;
    if Idx > FigureDimentions(1)*FigureDimentions(2)
        chART.save_figure([FigLabel,num2str(ParticipantIdx)], ResultsFolder, PlotProps)
        Idx = 1;
        figure('Units','normalized','OuterPosition',[0 0 1 1])
    elseif ParticipantIdx == numel(Participants)
        chART.save_figure([FigLabel, num2str(ParticipantIdx)], ResultsFolder, PlotProps)
    end
end


%% plot gamma topographies

close all


FigLabel = 'GammaTopo';
Idx = 1;
figure('Units','normalized','OuterPosition',[0 0 1 .6])
CLims = 'minmax';
for ParticipantIdx = 1:numel(Participants)
    for SessionIdx = 1:numel(Sessions)

        Data = squeeze(GammaTopographies(ParticipantIdx, SessionIdx, :));
        if ~all(isnan(Data))
            chART.sub_plot([], [numel(Sessions), 13], [SessionIdx, Idx], [], false, '', PlotProps)
            chART.plot.eeglab_topoplot(Data, Chanlocs, [], CLims, '', 'Linear', PlotProps)
            colorbar off
            if SessionIdx==1
                title(Participants(ParticipantIdx))
            end
        end


        if Idx > 13
            chART.save_figure([FigLabel,num2str(ParticipantIdx)], ResultsFolder, PlotProps)
            Idx = 1;
            figure('Units','normalized','OuterPosition',[0 0 1 .6])
        elseif ParticipantIdx == Parameters.Participants
            chART.save_figure([FigLabel, num2str(ParticipantIdx)], ResultsFolder, PlotProps)
        end
    end
    Idx = Idx+1;
end







