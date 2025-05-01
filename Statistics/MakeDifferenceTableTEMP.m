Folder = 'D:\Data\AllWake\Results\children-wake\poster\SleepWakeStatsStandaridzed\';
File= 'WakeSleepAllData.csv';

CSV = readtable(fullfile(Folder, File));


CSV2 = CSV(strcmp(CSV.Hour, 'eve'), :);


DiffFactors = {'Amplitude', 'Duration', 'Exponent', 'Offset', 'Sleep_Amplitude', 'Sleep_Slope_Matched'};


for RowIdx = 1:size(CSV2, 1)

    MatchRow = find(strcmp(CSV.Participant, CSV2.Participant{RowIdx}) &...
        strcmp(CSV.Session, CSV2.Session{RowIdx}) & strcmp(CSV.Hour, 'mor'));

for Factor = DiffFactors
    F = Factor{1};
    CSV2.(F)(RowIdx) = CSV.(F)(MatchRow) - CSV2.(F)(RowIdx);
    
end
end

writetable(CSV2, fullfile(Folder, 'WakeSleepDiff.csv'))