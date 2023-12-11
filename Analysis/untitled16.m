Window = [220 240];
WindowLength = 1;
MovingWindowSampleRate = .1;


% reduce data to make it faster
EEGRedux = pop_select(EEG, 'time', Window);
EEGRedux = pop_select(EEGRedux, 'channel', [6 11 36 51 68, 93]); % 6, Fz, C3, P3 O1

% run multitaper
Data = EEGRedux.data;
SampleRate = EEG.srate;

[Spectrum, Frequencies, Time] = cycy.utils.multitaper(Data, SampleRate, WindowLength, MovingWindowSampleRate);


PlotProps = chART.load_plot_properties({'LSM', 'Manuscript'});
ChannelLabels = {EEGRedux.chanlocs.labels};         

%%
figure('Units','normalized','OuterPosition',[0 0 1 1])
for ChannelIdx = 1:size(Spectrum, 1)
    LData = squeeze(log(Spectrum(ChannelIdx, :, :)));

    subplot(size(Spectrum, 1), 1, ChannelIdx)
    cycy.plot.time_frequency(LData, Frequencies, Time(end), 'contourf', [1 40], [-10, -4], 100)
    chART.set_axis_properties(PlotProps)
    colormap(PlotProps.Color.Maps.Linear)
    title(ChannelLabels(ChannelIdx))
end