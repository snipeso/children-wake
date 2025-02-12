function [chwaves, parameters, meanSW_Xamplitude, mean_numberofSW, meanSW_ascendslope, meanSW_descendslope] = kispi_detectSW_NPtoNP(data_SW, fs, visnum, artndxn, excludedchannels, epochl)

%% Slow wave detection as wave is considered to start with a negative peak and end with a negative peak
%
% Description : This function detects slow waves on EEG data filtered in
%               the slow-wave frequency range. The slow-wave detection is based on
%               consecutive zero-crossings. No frequency criteria are applied  
%
% Input
% ------
% data_SW  : Filtered EEG data (channels x samples) to run the wave analysis
% channels : Array indicating on which electrodes to run the analysis
% visnum  : The scoring array. Needs N2 and N3 to be -2 and -3
% artndxn  : The artfact correction file indicating the artefact-free epochs
% fs       : Sampling frequency
% savename : [path, name] to save the output
%
% Output
% -------
% chwaves   : 3-D matrix with 1st dimension the EEG channels, 2nd dimension
%             the detected slow waves and 3rd dimension the parameters of the waves
% parameters: np1 (sample of negative peak 1)
%             pcx (sample of positive zero-crossing)
%             ppx (sample of positive peak)
%             ncx (sample of negative zero-crossing)
%             np2 (sample of negative peak 2)
%             negamp1 (absolute amplitude of negative peak 1)
%             posamp (absolute amplitude of positive peak)
%             negamp2 (absolute amplitude of negative peak 2)
%             sleep(waveep_np1) (in which sleep stage did wave start)
% ampperc   : Mean (absolute) amplitude of SWs divided in percentiles
%             according to sorted wave amplitudes
% incperc   : Number of SWs in each percentile
% upperc    : Mean up slope (uV/sec) for negative deflection in each percentile
% dnperc    : Mean down slope (uV/sec) for negative deflection in each percentile


channels        = 1:size(data_SW,1);

artndxn(excludedchannels,:) = 0;            % set all epochs of the excluded channels to 0

ndxgoodch       = find(sum(artndxn,2) > 0); % find the indices of the good channels
ngoodch         = length(ndxgoodch);

badepochs       = find(sum(artndxn(ndxgoodch,:)) ~= ngoodch);   % find the epochs where not all good channels have good epochs
ndxdsleep       = find(visnum == -2 | visnum == -3);        % find indices of the epochs where the person is in deepsleep
notdeepsleep    = setdiff((1:length(visnum)),ndxdsleep);       % gives wake, n1, and rem epochs
rejep           = union(badepochs,notdeepsleep);                % find waking or rejected 20 sec epochs for all ch

totsamp_scor    = length(visnum)*epochl*fs;  % shorten data to only include scored data; each epoch is 20 seconds (1 epoch has 20 seconds so 1*20 = units in seconds), times the number of samples every second (seconds*sf)
data_SW         = data_SW(:,1:totsamp_scor);     % only take data until the last scored value

parameters = {'np1' 'pcx' 'ppx' 'ncx' 'np2' 'negamp1' 'posamp' 'negamp2' 'sleep(waveep_np1_np1)'};


% % factors1 = [-Inf; -Inf; 0.2; 0.4; 0.6; 0.8];
% % factors2 = [Inf; 0.2; 0.4; 0.6; 0.8; Inf];
% % 
% % meanSW_Xamplitude = NaN(size(channels,2),length(factors1));
% % mean_numberofSW = NaN(size(channels,2),length(factors1));
% % meanSW_ascendslope  = NaN(size(channels,2),length(factors1));
% % meanSW_descendslope  = NaN(size(channels,2),length(factors1));

%%%%%%%%%% start detecting slow waves %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% start channel loop -> all channels - one by one
for channels = 1:size(channels,2)
    
    current_channel = channels(channels);
    
    eval(['waves',num2str(current_channel),' = [];']);         % create empty matrix for waves for each channel
    
    data = data_SW(current_channel,1:totsamp_scor);
    
    disp(['Chan.',num2str(current_channel),', slow wave detection...'])
   
    
    pos_index           = zeros(length(data),1);
    pos_index(data > 0) = 1;                      % index of all positive and negative points. negative points have the value 0 and positive points the value 1.
    difference          = diff(pos_index);
    poscross            = find(difference == 1);  % index of all positive crossings
    negcross            = find(difference == -1); % index of all negative crossings
    clear pos_index difference;
    
    EEGder                = diff(data);
    pos_index             = zeros(length(EEGder),1);
    pos_index(EEGder > 0) = 1;  % index of all upgoing points
    difference            = diff(pos_index);
    clear EEGder
    peaks                      = find(difference == -1)+1; % find pos ZX of the derivative (the peaks)
    troughs                    = find(difference == 1)+1;  % find neg ZX of the derivative (the troughs)
    peaks(data(peaks) < 0)     = []; % delete peaks that are in negative halfwave
    troughs(data(troughs) > 0) = []; % delete troughs that are in positive halfwave
    
    
    if poscross(1) < negcross(1) % consider SWs starting with positive deflection
        poscross(1) = [];        % delete first positive crossing if it is smaller than first negative crossing
    end
    start = 1;
    clear difference pos_index
    
    % start wave detection loop
    for wndx = start:length(poscross)-1
        
        % first wave (first negative halfwave)
        wavest1   = negcross(wndx); % negative crossing before np1
        wavend1   = poscross(wndx); % positive crossing after np1
        negpeaks1 = troughs(troughs > wavest1 & troughs <= wavend1); % find troughs that are between wavest1 and waveend1 (1 means first negative half wave)
        np1       = negpeaks1(data(negpeaks1) == min(data(negpeaks1))); % most neg Peak
        if length(np1) > 1
            np1(1:end-1) = []; % keep last sample if more than 1 sample with minimal value
        end
        waveep_np1    = ceil(np1/(fs*epochl)); % in which epoch the wave belongs, evaluated at np1
        
        
        % second wave (second negative halfwave)
        wavest2   = negcross(wndx+1); % same as above for 2nd negative halfwave
        wavend2   = poscross(wndx+1);
        negpeaks2 = troughs(troughs > wavest2 & troughs <= wavend2);
        np2       = negpeaks2(data(negpeaks2) == min(data(negpeaks2))); % most neg Peak
        if length(np2) > 1
            np2(2:end) = []; % keep first sample if more than 1 sample with minimal value
        end
        waveep_np1_np2    = ceil(np2/(fs*epochl)); % in which epoch the wave belongs, evaluated at np1
        
        clear wavest1 wavend1 negpeaks1 wavest2 wavend2 negpeaks2
        
 
        % start rejection loop
        if ((~ismember(waveep_np1, rejep) && ~ismember(waveep_np1_np2, rejep))) % select wave only if np1 and np2 are not member of rejected epochs
            
            ncx      = negcross(wndx+1);
            pcx      = poscross(wndx);
            pospeaks = peaks(peaks > pcx & peaks < ncx);
            ppx      = pospeaks(data(pospeaks) == max(data(pospeaks))); % most positive Peak
            if length(ppx) > 1
                ppx(2:end) = []; % keep first sample if more than 1 sample with maximal value
            end
            
            negamp1 = abs(data(np1));
            negamp2 = abs(data(np2));
            posamp  = abs(data(ppx));
            
            if ~isempty(pospeaks)
                
                eval(['waves',num2str(current_channel),'=[waves',num2str(current_channel),'; np1 pcx ppx ncx np2 negamp1 posamp negamp2 visnum(waveep_np1)];']) 
            end
        end % end reject epochs loop
        
        clear waveep_np1 np1 np2 ncx pcx ppx negamp1 negamp2 posamp pospeaks
    end % end wave detect loop
    clear negcross peaks poscross troughs data
    
% %     %% Frequency criterion: Only waves between 0.5 und 2 Hz: pos half wave for criterion
% %     eval(['waves',num2str(jj),' = waves',num2str(jj),'(find(waves',num2str(jj),...
% %         '(:,9) <= -2 & (waves',num2str(jj),'(:,4) - waves',num2str(jj),...
% %         '(:,2))/fs >= 0.25 & (waves',num2str(jj),'(:,4) - waves',num2str(jj),...
% %         '(:,2))/fs <= 1),:);'])

    %% Percentile analysieren:

                                                                            %% Asked about column 7 being taken -- should it not be column 6 i.e., the negamp1
% %     for factorz = 1:length(factors1)
% %         
% %         factor1 = factors1(factorz);
% %         factor2 = factors2(factorz);
% %         eval(['amps = squeeze(waves',num2str(current_channel),'(:,7));']);
% %         amps    = sort(amps(~isnan(amps)));
% %         
% %         if length(amps) > 5
% %             if factor1 == -Inf
% %                 thresh1 = -Inf;
% %             else
% %                 thresh1 = amps(floor(length(amps)*factor1));
% %             end
% %             if factor2 == Inf
% %                 thresh2 = Inf;
% %             else
% %                 thresh2 = amps(floor(length(amps)*factor2));
% %             end
% %             
% %             eval(['chwaves1r = waves',num2str(current_channel),';']);
% %             SW_withinthresholds = find(chwaves1r(:,7) >= thresh1 & chwaves1r(:,7) < thresh2 & ~isnan(chwaves1r(:,7)));
% %             
% %             meanSW_ascendslope(current_channel,factorz) = mean(chwaves1r(SW_withinthresholds,7)*fs./(chwaves1r(SW_withinthresholds,3)-chwaves1r(SW_withinthresholds,2))); % Mean up slope (uV/sec) for negative deflection
% %             meanSW_descendslope(current_channel,factorz) = mean(chwaves1r(SW_withinthresholds,7)*fs./(chwaves1r(SW_withinthresholds,4)-chwaves1r(SW_withinthresholds,3))); % Mean down slope (uV/sec) for negative deflection
% %             
% %             meanSW_Xamplitude(current_channel,factorz) = mean(chwaves1r(SW_withinthresholds,7)); % Mean (abs) negative amplitude for each percentile (?V)
% %             mean_numberofSW(current_channel,factorz) = length(SW_withinthresholds);            % Number of SWs in each percentile
% %             
% %         else
% %             meanSW_ascendslope(current_channel,factorz)  = NaN;
% %             meanSW_descendslope(current_channel,factorz)  = NaN;
% %             meanSW_Xamplitude(current_channel,factorz) = NaN;
% %             mean_numberofSW(current_channel,factorz) = NaN;
% %         end
% %         clear SW_withinthresholds amps thresh1 thresh2 chwaves1r
% %     end
    
    
end


sizen_tot = [];
for channels = 1:size(channels,2)
    current_channel = channels(channels);
    eval(['sizen_tot=[sizen_tot;size(waves',num2str(current_channel),',1)];']);
end

chwaves = NaN(size(channels,2),max(sizen_tot),length(parameters));

for channels = 1:size(channels,2)
    current_channel = channels(channels);
    eval(['if ~isempty(waves',num2str(current_channel),');chwaves(current_channel,[1:size(waves',num2str(current_channel),',1)],[1:size(waves',num2str(current_channel),',2)])=waves',num2str(current_channel),';end;']);
end
