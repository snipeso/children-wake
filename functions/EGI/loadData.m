function [EEG, MAT] = loadData(Filename, Filepath)
% Reads either EGI or BrainAmp data, saves it into a EEGLAB structure. Uses
% both Fieldtrip and EEGLAB. Fileformats are a pain.
% from AllWake, Snipes 2023

% get channel locations file for 128 channels
load('StandardChanlocs128.mat', 'StandardChanlocs')

Filename = char(Filename);
Filepath = char(Filepath);

% get common part of filename
Core = extractBefore(Filename, '.');
Filetype = extractAfter(Filename, '.');
MAT = [Core, '.mat'];

switch Filetype
    case 'raw'
        % open the file
        fid = fopen(fullfile(Filepath, Filename), 'rb', 'b');
        [segInfo, dataFormat, header_array, EventCodes, Samp_Rate, NChan, scale, NSamp, NEvent] = readRAWFileHeader(fid);
        fclose(fid);
        
        
        % save all info to EEG structure
        EEG.matname = MAT;
        EEG.filename = Filename;
        EEG.filepath = Filepath;
        
        EEG.nbchan = 128;
        EEG.pnts = NSamp;
        EEG.srate = Samp_Rate;
        EEG.scale = scale;
        EEG.seginfo = segInfo;
        EEG.dataformat = dataFormat;
        EEG.headerarray = header_array;
        EEG.EventCodes = EventCodes;
        EEG.nevent = NEvent;
        EEG.event = [];
        EEG.trials = 0;
        EEG.nevent = NEvent; % TODO: 
        EEG.icawinv = [];
        EEG.icaweights = [];
        EEG.icasphere = [];
        EEG.xmax = EEG.pnts;
        EEG.xmin = 0;
        EEG.icaact = [];
        
        % convert and load to structure the EEG data
        Data = loadEGIBigRaw(fullfile(Filepath, Filename),1:NChan);
        EEG.data = Data(1:128, :);

        % % get events using fieldtrip
        % events_ft = ft_read_event(fullfile(Filepath, Filename)); % triggers
        % events_el = fieldtrip2eeglab_events(events_ft);
        % EEG.event = events_el;


    case 'eeg'
        VHDR = [Core, '.vhdr'];
        EEG = pop_loadbv(Filepath, VHDR); % If this doesn't work, it's because you didn't download the BV extention yet
end

% update EEG structure
EEG.ref = 'CZ';
EEG.chanlocs = StandardChanlocs;
EEG.setname = replace(MAT, '.mat', '.set');
EEG.info.oldname = Filename;
EEG.info.oldpath = Filepath;

EEG = eeg_checkset(EEG);