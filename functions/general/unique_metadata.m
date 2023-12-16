function UniqueMetadata = unique_metadata(Metadata, Column)
arguments
    Metadata
    Column = 'Participant';
end

NotOutcomeVariables = {'Dataset', 'Participant', 'Session', 'Sex', 'Handedness', 'Group', 'Subgroup', 'Condition', 'Hour', 'Task', 'Index'};
AllVariables = Metadata.Properties.VariableNames;
OutcomeVariables = setdiff(AllVariables, NotOutcomeVariables);

[UniqueItems, UniqueIndx] = unique(Metadata.(Column));

try
    Nans = isnan(UniqueItems);
    UniqueItems(Nans) = [];
    UniqueIndx(Nans) = [];
end

UniqueMetadata = Metadata(UniqueIndx, :);

for ItemIdx = 1:numel(UniqueItems)
    for Variable = OutcomeVariables'
        UniqueMetadata.(Variable{1})(ItemIdx) = mean(Metadata.(Variable{1})(ismember(Metadata.(Column), UniqueItems(ItemIdx))), 'omitnan');
    end
end