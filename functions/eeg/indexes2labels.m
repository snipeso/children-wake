function Labels = indexes2labels(Indexes, Chanlocs)
% Converts numeric channel indices to EEGLAB labels.

AllLabels =  string({Chanlocs.labels});
AllLabels(strcmpi(AllLabels, 'CZ')) = "129";
Labels = AllLabels(Indexes);

Labels = str2double(Labels);
