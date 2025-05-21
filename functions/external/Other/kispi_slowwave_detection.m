function [SW] = kispi_slowwave_detection(EegData, SampRate, WaveStart, CleanEpochs, EpochLength, QualityCheck)
arguments
    EegData {mustBeNumeric}
    SampRate {mustBeNumeric}
    WaveStart {mustBeMember(WaveStart, {'Peaks', 'Troughs', 'PosZeroCrossings', 'NegZeroCrossings'})} = 'NegZeroCrossings'
    CleanEpochs (1,:) {mustBeNumeric} = []
    EpochLength (1,:) {mustBeNumeric} = 30
    QualityCheck {mustBeMember(QualityCheck, ['Yes', 'No'])} = 'No'
end


EegData = validateEegData(EegData);

%   Description:
%   This function detects and characterizes slow waves in EEG data. It operates
%   across multiple channels and can detect slow waves of different start
%   points, that is, peaks, troughs, or zero crossings.
%
%   *** Inputs:
%   EegData     - EEG data with all channels of interest (it is recommended
%                 to use bandpass filtered data within a reasonable frequency range (e.g. 0.5 to 4 Hz)) 
%   SampRate    - Sampling rate of the data
%   WaveStart   - Method for defining the start of slow waves:
%                   'Peaks', 'Troughs', 'PosZeroCrossings', 'NegZeroCrossings'
%                 (default: 'NegZeroCrossing').
%   CleanEpochs - Indices of epochs considered to be clean NREM sleep periods.
%                 Default is an empty array (no clean epochs defined).
%   EpochLength - Length of epochs in seconds (default: 30 seconds).
%   QualityCheck- Option to perform a small quality check figure ('Yes' or 'No').
%                 Default is 'No'.
%
%   Outputs:
%   SW          - Structure containing detected slow waves with detailed
%                 parameters
%


% Initialize variable
nChans           = size(EegData, 1);
SW.waves(nChans) = struct();

% Loop over channels
tic; fprintf('\n Channel:')
for ch = 1 : nChans
    fprintf(' %d', ch)

    % *** Section defining waves
    % First compute the positive and negative zero crossings, as well as the
    % peaks and troughs in the data needed to define what is a slow wave

    % Extract channel data
    data = EegData(ch, :);

    % Positive and Negative Zero Crossings
    pos_index  = data > 0;                      % Logical index of all positive points in the EEG data
    difference = diff(pos_index);               % Locate transitions from negative to positive and vice versa
    poscross   = find(difference == 1);         % Indices of the first positive points (zero-crossings)
    negcross   = find(difference == -1);        % Indices of the first negative points (zero-crossings)
    clear pos_index difference

    % Troughs and Peaks in the Derivative
    
    pos_index  = diff(data) > 0;                % all ascending points in the data
    difference = diff(pos_index);               % Locate transitions from ascending to decending parts
    peaks      = find(difference == -1) + 1;    % Indices of all peaks 
    troughs    = find(difference == 1) + 1;     % Indices of all troughs 
    peaks(data(peaks) < 0)     = [];            % Remove peaks below zero in the original data
    troughs(data(troughs) > 0) = [];            % Remove troughs above zero in the original data

    % If channel has flat line, skip, otherwise script will stop
    if all(data == 0)
        fprintf('\n Ups! The signal of channel %d is flat (only zeros), skipping channel ..', ch)
        fprintf('\n Channel')
        continue
    end

    % Define start and end of waves
    switch WaveStart
        case 'NegZeroCrossings'
            SW.waves(ch).wave_start      = negcross(1:end-1);
            SW.waves(ch).wave_stop       = negcross(2:end);
            SW.waves(ch).mid_pos_xing    = poscross(poscross > negcross(1) & poscross < negcross(end));    % All positive zero crossings must come after the wave start
            
            waves = arrayfun(@(x, y) data(x:y), SW.waves(ch).wave_start, SW.waves(ch).wave_stop, 'Uni', 0); % Takes the values from x (1st negative crossing) to y (second negative crossing) in the data variable
           
            SW.waves(ch).amp_peak       = cellfun(@(x) max(x), waves);
            SW.waves(ch).amp_trough     = cellfun(@(x) min(x), waves);
            SW.waves(ch).ndx_peak       = SW.waves(ch).wave_start + single(cellfun(@(x) find(x == max(x), 1, 'last' ), waves));  %%%%%% if there is more than one max it takes the lates
            SW.waves(ch).ndx_trough     = SW.waves(ch).wave_start + single(cellfun(@(x) find(x == min(x), 1, 'last'), waves));   %%%%%% if there is more than one min it takes the lates
            SW.waves(ch).neg_period     = (SW.waves(ch).ndx_trough-SW.waves(ch).wave_start)/SampRate;
            SW.waves(ch).neg_slope      = SW.waves(ch).amp_trough./(SW.waves(ch).neg_period);
            
            clear waves

        case 'PosZeroCrossings'
            SW.waves(ch).wave_start      = poscross(1:end-1);
            SW.waves(ch).wave_stop       = poscross(2:end);
            SW.waves(ch).mid_neg_xing    = negcross(negcross > poscross(1) & negcross < poscross(end));

            waves = arrayfun(@(x, y) data(x:y), SW.waves(ch).wave_start, SW.waves(ch).wave_stop, 'Uni', 0); % Takes the values from x (1st negative crossing) to y (second negative crossing) in the data variable

            SW.waves(ch).amp_peak       = cellfun(@(x) max(x), waves);
            SW.waves(ch).amp_trough     = cellfun(@(x) min(x), waves);
            SW.waves(ch).ndx_peak       = SW.waves(ch).wave_start + single(cellfun(@(x) find(x == max(x), 1, 'last' ), waves));  %%%%%% if there is more than one max it takes the lates
            SW.waves(ch).ndx_trough     = SW.waves(ch).wave_start + single(cellfun(@(x) find(x == min(x), 1, 'last'), waves));   %%%%%% if there is more than one min it takes the lates
            clear waves

        case 'Peaks'
            poshalfwave_start           = poscross(1:end);
            poshalfwave_stop            = negcross(1:end);
          
            if poshalfwave_stop(1) < poshalfwave_start(1) 
                poshalfwave_stop(1) = [];
            end

            if poshalfwave_start(end) > poshalfwave_stop(end)
                 poshalfwave_start(end) = [];
            end

            poshalfwaves = arrayfun(@(x, y) data(x:y), poshalfwave_start,  poshalfwave_stop, 'Uni', 0); % Takes the values from x (poshalfwave start) to y (poshalfwave end) in the data variable
            
            peak_posthalfwaves         = cellfun(@(x) max(x), poshalfwaves);
           
            SW.waves(ch).amp_peak1     = peak_posthalfwaves(1:1:end-1);
            SW.waves(ch).amp_peak2     = peak_posthalfwaves(2:1:end);
            wave_start_end_ndx         = cellfun(@(x) find(x == max(x), 1, 'last' ), poshalfwaves);
            SW.waves(ch).wave_start    = poshalfwave_start(1:1:end-1) + wave_start_end_ndx(1:1:end-1);
            SW.waves(ch).wave_stop     = poshalfwave_start(2:1:end) + wave_start_end_ndx(2:1:end);

            neghalfwave_start           = poshalfwave_stop(1:end-1);
            neghalfwave_stop            = poshalfwave_start(2:end);

            neghalfwaves = arrayfun(@(x, y) data(x:y), neghalfwave_start,  neghalfwave_stop, 'Uni', 0); % Takes the values from x (poshalfwave start) to y (poshalfwave end) in the data variable
            
            SW.waves(ch).amp_trough      = cellfun(@(x) min(x), neghalfwaves);
            SW.waves(ch).ndx_trough      = neghalfwave_start + single(cellfun(@(x) find(x == min(x), 1, 'last' ), neghalfwaves));  %%%%%% if there is more than one min it takes the lates
            SW.waves(ch).neg_xing        = neghalfwave_start; %%%% negative crossing
            SW.waves(ch).pos_xing        = neghalfwave_stop; %%%% positive crossing

            clear poshalfwave_start poshalfwave_stop poshalfwaves peak_posthalfwaves wave_start_end_ndx neghalfwave_start neghalfwave_stop neghalfwaves

        case 'Troughs'

            neghalfwave_start           = negcross(1:end);
            neghalfwave_stop            = poscross(1:end);
          
            if neghalfwave_stop(1) < neghalfwave_start(1) 
                neghalfwave_stop(1) = [];
            end

            if neghalfwave_start(end) > neghalfwave_stop(end)
                 neghalfwave_start(end) = [];
            end

            neghalfwaves = arrayfun(@(x, y) data(x:y), neghalfwave_start,  neghalfwave_stop, 'Uni', 0); % Takes the values from x (neghalfwave start) to y (neghalfwave end) in the data variable
            
            troughs_negthalfwaves      = cellfun(@(x) min(x), neghalfwaves);
           
            SW.waves(ch).amp_trough1   = troughs_negthalfwaves(1:1:end-1);
            SW.waves(ch).amp_trough2   = troughs_negthalfwaves(2:1:end);
            wave_start_end_ndx         = cellfun(@(x) find(x == min(x), 1, 'last' ), neghalfwaves);
            SW.waves(ch).wave_start    = neghalfwave_start(1:1:end-1) + wave_start_end_ndx(1:1:end-1);
            SW.waves(ch).wave_stop     = neghalfwave_start(2:1:end) + wave_start_end_ndx(2:1:end);

            poshalfwave_start           = neghalfwave_stop(1:end-1);
            poshalfwave_stop            = neghalfwave_start(2:end);

            poshalfwaves = arrayfun(@(x, y) data(x:y), poshalfwave_start,  poshalfwave_stop, 'Uni', 0); % Takes the values from x (poshalfwave start) to y (poshalfwave end) in the data variable
            
            SW.waves(ch).amp_peak        = cellfun(@(x) max(x), poshalfwaves);
            SW.waves(ch).ndx_peak        = poshalfwave_start + single(cellfun(@(x) find(x == max(x), 1, 'last' ), poshalfwaves));  %%%%%% if there is more than one min it takes the lates
            SW.waves(ch).pos_xing        = poshalfwave_start; %%%% positive crossing
            SW.waves(ch).neg_xing        = poshalfwave_stop; %%%%  negative crossing

            clear neghalfwave_start neghalfwave_stop neghalfwaves troughs_negthalfwaves wave_start_end_ndx poshalfwave_start poshalfwave_stop poshalfwaves

    end

    % Remove waves not in clean NREM epochs

    if ~isempty(CleanEpochs)
        clean_epochs_start = (CleanEpochs - 1) * EpochLength * SampRate + 1;
        clean_epochs_end = CleanEpochs * EpochLength * SampRate;
        clean_epoch_intervals = [clean_epochs_start',clean_epochs_end'];
        if ~isempty(CleanEpochs)
            % Create logical index for valid waves
            valid_waves = false(size(SW.waves(ch).wave_start));

            % Check each wave for inclusion in any clean epoch
            for wave_idx = 1 : length(SW.waves(ch).wave_start)
                wave_start = SW.waves(ch).wave_start(wave_idx);
                wave_stop = SW.waves(ch).wave_stop(wave_idx);
                for epoch_idx = 1 : size(clean_epoch_intervals, 1)
                    if wave_start >= clean_epoch_intervals(epoch_idx, 1) && wave_stop <= clean_epoch_intervals(epoch_idx, 2)
                        valid_waves(wave_idx) = true;
                        break;
                    end
                end
            end

            % Filter the waves
            fields = fieldnames(SW.waves(ch));
            for f = 1 : numel(fields)
                SW.waves(ch).(fields{f}) = SW.waves(ch).(fields{f})(valid_waves);
            end
        end
    end


   
    if strcmp(QualityCheck,'Yes')

        switch WaveStart

        case 'NegZeroCrossings'
  
            figure;
            hold on;
            WavesToPlot = 10:20;
            PlotEEGStart = (SW.waves(ch).wave_start(WavesToPlot(1)));
            PlotEEGEnd = (SW.waves(ch).wave_stop(WavesToPlot(end)));
            plot((PlotEEGStart:PlotEEGEnd)/SampRate, data(PlotEEGStart:PlotEEGEnd))
            
            wave_start_x = SW.waves(ch).wave_start(WavesToPlot)/SampRate;
            wave_start_y = data(SW.waves(ch).wave_start(WavesToPlot));
            wave_stop_x = SW.waves(ch).wave_stop(WavesToPlot)/SampRate;
            wave_stop_y = data(SW.waves(ch).wave_stop(WavesToPlot));
            
            wave_posAmp_x = SW.waves(ch).ndx_peak(WavesToPlot)/SampRate;
            wave_posAmp_y = SW.waves(ch).amp_peak(WavesToPlot);
            
            wave_negAmp_x = SW.waves(ch).ndx_trough(WavesToPlot)/SampRate;
            wave_negAmp_y = SW.waves(ch).amp_trough(WavesToPlot);
            
            wave_posx_x = SW.waves(ch).mid_pos_xing(WavesToPlot)/SampRate;
            wave_posx_y = data(SW.waves(ch).mid_pos_xing(WavesToPlot));
            
            plot(wave_start_x, wave_start_y, 'k^', 'DisplayName', 'Wave start')
            plot(wave_stop_x, wave_stop_y, 'rv', 'DisplayName', 'Wave stop')
            plot([wave_posAmp_x; wave_posAmp_x],[zeros(1,numel(wave_posAmp_x));wave_posAmp_y],'g--', 'DisplayName', 'pos Amp', 'HandleVisibility', 'off')
            plot([wave_posAmp_x(1); wave_posAmp_x(1)],[0; wave_posAmp_y(1)],'g--', 'DisplayName', 'pos Amp')  % Plot one with legend
            plot([wave_negAmp_x; wave_negAmp_x],[zeros(1,numel(wave_negAmp_x));wave_negAmp_y],'c--','DisplayName', 'neg Amp', 'HandleVisibility', 'off')
            plot([wave_negAmp_x(1); wave_negAmp_x(1)],[0; wave_negAmp_y(1)],'c--', 'DisplayName', 'neg Amp')  % Plot one with legend
            plot(wave_posx_x, wave_posx_y, 'm*', 'DisplayName', 'mid pos x-ing')
            yline(0, 'k--', 'HandleVisibility', 'off')
            legend()
            title(['Wave start: ',WaveStart])


           case 'PosZeroCrossings'

            figure;
            hold on;
            WavesToPlot = 10:20;
            PlotEEGStart = (SW.waves(ch).wave_start(WavesToPlot(1)));
            PlotEEGEnd = (SW.waves(ch).wave_stop(WavesToPlot(end)));
            plot((PlotEEGStart:PlotEEGEnd)/SampRate, data(PlotEEGStart:PlotEEGEnd))
            
            wave_start_x = SW.waves(ch).wave_start(WavesToPlot)/SampRate;
            wave_start_y = data(SW.waves(ch).wave_start(WavesToPlot));
            wave_stop_x = SW.waves(ch).wave_stop(WavesToPlot)/SampRate;
            wave_stop_y = data(SW.waves(ch).wave_stop(WavesToPlot));
            
            wave_posAmp_x = SW.waves(ch).ndx_peak(WavesToPlot)/SampRate;
            wave_posAmp_y = SW.waves(ch).amp_peak(WavesToPlot);
            
            wave_negAmp_x = SW.waves(ch).ndx_trough(WavesToPlot)/SampRate;
            wave_negAmp_y = SW.waves(ch).amp_trough(WavesToPlot);
            
            wave_negx_x = SW.waves(ch).mid_neg_xing(WavesToPlot)/SampRate;
            wave_negx_y = data(SW.waves(ch).mid_neg_xing(WavesToPlot));
            
            plot(wave_start_x, wave_start_y, 'k^', 'DisplayName', 'Wave start')
            plot(wave_stop_x, wave_stop_y, 'rv', 'DisplayName', 'Wave stop')
            plot([wave_posAmp_x; wave_posAmp_x],[zeros(1,numel(wave_posAmp_x));wave_posAmp_y],'g--', 'DisplayName', 'pos Amp', 'HandleVisibility', 'off')
            plot([wave_posAmp_x(1); wave_posAmp_x(1)],[0; wave_posAmp_y(1)],'g--', 'DisplayName', 'pos Amp')  % Plot one with legend
            plot([wave_negAmp_x; wave_negAmp_x],[zeros(1,numel(wave_negAmp_x));wave_negAmp_y],'c--','DisplayName', 'neg Amp', 'HandleVisibility', 'off')
            plot([wave_negAmp_x(1); wave_negAmp_x(1)],[0; wave_negAmp_y(1)],'c--', 'DisplayName', 'neg Amp')  % Plot one with legend
            plot(wave_negx_x, wave_negx_y, 'm*', 'DisplayName', 'mid neg x-ing')
            yline(0, 'k--', 'HandleVisibility', 'off')
            legend()
            title(['Wave start: ',WaveStart])

           case 'Peaks'

            figure;
            hold on;
            WavesToPlot = 10:20;
            PlotEEGStart = (SW.waves(ch).wave_start(WavesToPlot(1)));
            PlotEEGEnd = (SW.waves(ch).wave_stop(WavesToPlot(end)));
            plot((PlotEEGStart:PlotEEGEnd)/SampRate, data(PlotEEGStart:PlotEEGEnd))
            
            wave_start_x = SW.waves(ch).wave_start(WavesToPlot)/SampRate;
            wave_start_y = data(SW.waves(ch).wave_start(WavesToPlot));
            wave_stop_x = SW.waves(ch).wave_stop(WavesToPlot)/SampRate;
            wave_stop_y = data(SW.waves(ch).wave_stop(WavesToPlot));
            
            wave_posAmp_1_x = SW.waves(ch).wave_start(WavesToPlot)/SampRate;
            wave_posAmp_1_y = SW.waves(ch).amp_peak1(WavesToPlot);
                      
            wave_posAmp_2_x = SW.waves(ch).wave_stop(WavesToPlot)/SampRate;
            wave_posAmp_2_y = SW.waves(ch).amp_peak2(WavesToPlot);            
            
            wave_Amp_trough_x = SW.waves(ch).ndx_trough(WavesToPlot)/SampRate;
            wave_Amp_trough_y = SW.waves(ch).amp_trough(WavesToPlot);          
                       
            wave_negx_x = SW.waves(ch).neg_xing(WavesToPlot)/SampRate;
            wave_negx_y = data(SW.waves(ch).neg_xing(WavesToPlot));
            
            wave_posx_x = SW.waves(ch).pos_xing(WavesToPlot)/SampRate;
            wave_posx_y = data(SW.waves(ch).pos_xing(WavesToPlot));
                     
            plot(wave_start_x, wave_start_y, 'k^', 'DisplayName', 'Wave start')
            plot(wave_stop_x, wave_stop_y, 'rv', 'DisplayName', 'Wave stop') 
            plot([wave_posAmp_1_x; wave_posAmp_1_x],[zeros(1,numel(wave_posAmp_1_x));wave_posAmp_1_y],'g', 'DisplayName', 'pos Amp', 'HandleVisibility', 'off')
            plot([wave_posAmp_1_x(1); wave_posAmp_1_x(1)],[0; wave_posAmp_1_y(1)],'g', 'DisplayName', 'pos Amp1')  % Plot one with legend
            plot([wave_posAmp_2_x; wave_posAmp_2_x],[zeros(1,numel(wave_posAmp_2_x));wave_posAmp_2_y],'y--', 'DisplayName', 'pos Amp', 'HandleVisibility', 'off')
            plot([wave_posAmp_2_x(1); wave_posAmp_2_x(1)],[0; wave_posAmp_2_y(1)],'y--', 'DisplayName', 'pos Amp2')  % Plot one with legend
            plot([wave_Amp_trough_x; wave_Amp_trough_x],[zeros(1,numel(wave_Amp_trough_x));wave_Amp_trough_y],'c--','DisplayName', 'neg Amp', 'HandleVisibility', 'off')
            plot([wave_Amp_trough_x(1); wave_Amp_trough_x(1)],[0; wave_Amp_trough_y(1)],'c--', 'DisplayName', 'neg Amp')  % Plot one with legend
            plot(wave_negx_x, wave_negx_y, 'm*', 'DisplayName', 'neg x-ing')
            plot(wave_posx_x, wave_posx_y, 'b*', 'DisplayName', 'pos x-ing')
            yline(0, 'k--', 'HandleVisibility', 'off')
            legend()
            title(['Wave start: ',WaveStart])

           case 'Troughs'

            figure;
            hold on;
            WavesToPlot = 10:20;
            PlotEEGStart = (SW.waves(ch).wave_start(WavesToPlot(1)));
            PlotEEGEnd = (SW.waves(ch).wave_stop(WavesToPlot(end)));
            plot((PlotEEGStart:PlotEEGEnd)/SampRate, data(PlotEEGStart:PlotEEGEnd))
            
            wave_start_x = SW.waves(ch).wave_start(WavesToPlot)/SampRate;
            wave_start_y = data(SW.waves(ch).wave_start(WavesToPlot));
            wave_stop_x = SW.waves(ch).wave_stop(WavesToPlot)/SampRate;
            wave_stop_y = data(SW.waves(ch).wave_stop(WavesToPlot));
            
            wave_negAmp_1_x = SW.waves(ch).wave_start(WavesToPlot)/SampRate;
            wave_negAmp_1_y = SW.waves(ch).amp_trough1(WavesToPlot);
                      
            wave_negAmp_2_x = SW.waves(ch).wave_stop(WavesToPlot)/SampRate;
            wave_negAmp_2_y = SW.waves(ch).amp_trough2(WavesToPlot);            
            
            wave_Amp_peak_x = SW.waves(ch).ndx_peak(WavesToPlot)/SampRate;
            wave_Amp_peak_y = SW.waves(ch).amp_peak(WavesToPlot);          
                       
            wave_negx_x = SW.waves(ch).neg_xing(WavesToPlot)/SampRate;
            wave_negx_y = data(SW.waves(ch).neg_xing(WavesToPlot));
            
            wave_posx_x = SW.waves(ch).pos_xing(WavesToPlot)/SampRate;
            wave_posx_y = data(SW.waves(ch).pos_xing(WavesToPlot));
                     
            plot(wave_start_x, wave_start_y, 'k^', 'DisplayName', 'Wave start')
            plot(wave_stop_x, wave_stop_y, 'rv', 'DisplayName', 'Wave stop') 
            plot([wave_negAmp_1_x; wave_negAmp_1_x],[zeros(1,numel(wave_negAmp_1_x));wave_negAmp_1_y],'g', 'DisplayName', 'pos Amp', 'HandleVisibility', 'off')
            plot([wave_negAmp_1_x(1); wave_negAmp_1_x(1)],[0; wave_negAmp_1_y(1)],'g', 'DisplayName', 'neg Amp1')  % Plot one with legend
            plot([wave_negAmp_2_x; wave_negAmp_2_x],[zeros(1,numel(wave_negAmp_2_x));wave_negAmp_2_y],'y--', 'DisplayName', 'pos Amp', 'HandleVisibility', 'off')
            plot([wave_negAmp_2_x(1); wave_negAmp_2_x(1)],[0; wave_negAmp_2_y(1)],'y--', 'DisplayName', 'neg Amp2')  % Plot one with legend
            plot([wave_Amp_peak_x; wave_Amp_peak_x],[zeros(1,numel(wave_Amp_peak_x));wave_Amp_peak_y],'c--','DisplayName', 'neg Amp', 'HandleVisibility', 'off')
            plot([wave_Amp_peak_x(1); wave_Amp_peak_x(1)],[0; wave_Amp_peak_y(1)],'c--', 'DisplayName', 'pos Amp')  % Plot one with legend
            plot(wave_negx_x, wave_negx_y, 'm*', 'DisplayName', 'neg x-ing')
            plot(wave_posx_x, wave_posx_y, 'b*', 'DisplayName', 'pos x-ing')
            yline(0, 'k--', 'HandleVisibility', 'off')
            legend()
            title(['Wave start: ',WaveStart])
        end
        
    end


end % for channels!

% Store some relevant parameters!
SW.WaveStart = WaveStart;
SW.SampRate = SampRate;
SW.Epochs = CleanEpochs;
SW.EpochLength = EpochLength;

% Print time needed
TimeNeeded = toc;
fprintf('\nSlow-wave detection took %.2f minutes', TimeNeeded/60)

end

function EegData = validateEegData(EegData)
    if size(EegData,1)>size(EegData,2) % Orient the data channels x data points
        EegData=EegData';
        warning('The data has to stored as channels x sampels. Your data has now be swiched')
    end
end