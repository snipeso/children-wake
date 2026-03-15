Codes = readtable(fullfile('D:\Data\AllWake\Metadata', 'ParticipantCodes.csv'));
Metadata = readtable(fullfile('D:\Data\AllWake\Metadata', 'Metadata_Children_Wake.csv'));
% CSV = readtable('D:\Data\AllWake\ValeriaSlopes\Table1_slope_FHLH_allelectrodes.xlsx');
% CSV = readtable('D:\Data\AllWake\ValeriaSlopes\Table1_amp_FHLH_allelectrodes.xlsx');
CSV = readtable('D:\Data\AllWake\ValeriaSlopes\TableS2_lme_allwaves_allch.csv');


RecodedData = table();
for ParticipantIdx = 1:size(Codes, 1)
    ParticipantID = Codes.OldName{ParticipantIdx};
    Data = CSV(strcmp(CSV.subject, ParticipantID), :);
    if isempty(Data)
        continue
    end

    NewID = Codes.NewName{ParticipantIdx};
    Data.subject = repmat({NewID}, size(Data, 1), 1);
     Data.time(strcmp(Data.time, 'FH')) = repmat({1}, nnz(strcmp(Data.time, 'FH')), 1);
      Data.time(strcmp(Data.time, 'LH')) = repmat({2}, nnz(strcmp(Data.time, 'LH')), 1);
    RecodedData = [RecodedData; Data];
end


%%

for RowIdx = 1:size(RecodedData, 1)
    Session = Metadata.Session(strcmp(Metadata.Participant, RecodedData.subject{RowIdx}) & strcmp(Metadata.Condition, 'base'));

 Group = Metadata.Group(strcmp(Metadata.Participant, RecodedData.subject{RowIdx}) & strcmp(Metadata.Condition, 'base'));
    RecodedData.session(RowIdx) = Session(end);
    RecodedData.group(RowIdx) = Group(end);
end

writetable(RecodedData, fullfile('D:\Data\AllWake\ValeriaSlopes\', 'RecodedAmplitudesUnmatched.csv'))