function UniqueMetadata = unique_metadata(Metadata, Column)
arguments
    Metadata
    Column = 'Participant';
end


OutcomeVariables = get_outcome_variables(Metadata);
[UniqueItems, UniqueIndx] = unique(Metadata.(Column));

try
    Nans = isnan(UniqueItems);
    UniqueItems(Nans) = [];
    UniqueIndx(Nans) = [];
end

UniqueMetadata = Metadata(UniqueIndx, :);

for ItemIdx = 1:numel(UniqueItems)
    for Variable = OutcomeVariables'
        try
        UniqueMetadata.(Variable{1})(ItemIdx) = mean(Metadata.(Variable{1})(ismember(Metadata.(Column), UniqueItems(ItemIdx))), 'omitnan');
        catch
            a=1
        end
    end
end