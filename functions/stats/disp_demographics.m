function disp_demographics(Metadata, CategoryColumn)
% display proportion of male/female, age ranges, handedness, group,
% subgroup, and dataset, split by unique elements in Category


Categories = unique(Metadata.(CategoryColumn));

clc
for Category = Categories'
    disp('_________________')
    disp(Category{1})

    TempMetadata = Metadata(ismember(Metadata.(CategoryColumn), Category), :);


    disp(['N = ', num2str(numel(unique(TempMetadata.Participant)))])
    
    disp('***')
    disp('Dataset:')
    tabulate(TempMetadata.Dataset)

    disp('***')
        disp('Sex:')
    tabulate(TempMetadata.Sex)

    disp('***')
            disp('Handedness:')
    tabulate(TempMetadata.Handedness)

        disp('***')
        disp('Group:')
    tabulate(TempMetadata.Group)

    disp('***')
        disp('Subgoup:')
    tabulate(TempMetadata.Subgroup)

        disp('***')
    disp('Ages: ')
    disp([num2str(min(TempMetadata.Age), '%.1f'),'-', num2str(max(TempMetadata.Age), '%.1f'), ...
        '; mean: ', num2str(mean(TempMetadata.Age), '%.1f'), '+-', num2str(std(TempMetadata.Age), '%.1f')])
end