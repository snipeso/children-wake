% first script is for converting eeg files so there's.raw a .mat with the data.
close all
clear
clc


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parameters

P = prepParameters();
Paths = P.Paths;
% Datasets = P.Datasets;
Datasets = {'Providence'};

Refresh = true;

Template = '000';
Ignore = {};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


for Indx_D = 1:numel(Datasets)

    % Folders where raw data is located
    [Subfolders, Participants] = gather_folder_paths(fullfile(Paths.Datasets, Datasets{Indx_D}), ...
        Template, false, Ignore);


    %%% loop through all EEG folders, and convert whatever files possible
    for Indx_P = 1:size(Participants, 1) % loop through participants
        for Indx_S = 1:size(Subfolders, 1) % loop through all subfolders

            % get path
            Path = fullfile(Paths.Datasets, Datasets{Indx_D}, ...
                Participants{Indx_P}, Subfolders{Indx_S});

            % skip rest if path not found
            if ~exist(Path, 'dir')
                warning([deblank(Path), ' does not exist'])
                continue
            end

            % if does not contain EEG, then skip
            Content = list_filenames(Path);
            RAW = Content(contains(Content, '.raw'));
            if numel(RAW) < 1
                warning([Path, ' is missing EEG files'])
                continue
            end

            for Indx_F = 1:numel(RAW)

                % if file exists, and don't want to refresh, then skip rest of code
                if ~Refresh &&  any(contains(cellstr(Content), replace(RAW(Indx_F, :), '.raw', '.mat')))
                    disp(['***********', 'Already did ', RAW(Indx_F, :), '***********'])
                    continue
                end

                % convert EEG file
                [EEG, MAT] = load_eeg_data(RAW(Indx_F, :), Path);

                % fix very stupid channel indexing mistake
                if strcmp(Datasets{Indx_D}, 'Providence')
                    NewData = [EEG.data(1:64, :); EEG.data(97:128, :); EEG.data(65:96, :)];
                    EEG.data = NewData;
                end

                % save
                try
                    save(fullfile(Path, MAT), 'EEG')
                catch
                    warning(['Failed to save ', RAW(Indx_F, :)])
                end
            end
        end
        disp(['Finished ',  Participants{Indx_P}])
    end
    disp(['Finished ' Datasets{Indx_D}])
end