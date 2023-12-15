


% paired t-tests across channels for ADHD and controls
Ages = [7.5 14];

Measures = fieldnames(BurstInformationTopography);

nMeasures = numel(Measures);

TempMetadata = Metadata(Metadata.Age >=Ages(1) & Metadata.Age<=Ages(2), :);

% [Participants, UniqueIndx] = unique(Metadata.Participant);
% UniqueMetadata = Metadata(UniqueIndx, :);
% MetadataPatients = UniqueMetadata(ismember(UniqueMetadata.Subgroup, [1, 2 3 4]), :);
%
% MetadataControls = OvernightMetadata(OvernightMetadata.Subgroup==5, :);
%
% [MetadataPatients] = match_participants(MetadataPatients, MetadataControls);

OvernightMetadata = overnight_changes(TempMetadata);
Groups = {'HC', 'ADHD'};
CLims = [-1 1];
PlotProps.PlotN = true;

figure('Units','normalized','Position', [0 0 .35 .15*nMeasures])
for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    % overnight changes
    for GroupIdx = 1:numel(Groups)
        EveningIndexes = OvernightMetadata.EveningIndexes(strcmp(OvernightMetadata.Group, Groups{GroupIdx}));
        MorningIndexes = OvernightMetadata.MorningIndexes(strcmp(OvernightMetadata.Group, Groups{GroupIdx}));
        Evening = Topographies(EveningIndexes, :);
        Morning = Topographies(MorningIndexes, :);

        Evening = average_by_column(TempMetadata, EveningIndexes, Evening, 'Participant');
        Morning = average_by_column(TempMetadata, MorningIndexes, Morning, 'Participant');

        chART.sub_plot([], [nMeasures, 2], [MeasuresIdx, GroupIdx], [], false, '', PlotProps);
        plot_topography_difference(Evening, Morning, Chanlocs, CLims, Parameters.Stats, PlotProps)
        colorbar off


        X = get(gca, 'XLim');
        Y = get(gca, 'YLim');
        if GroupIdx ==1
            text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasuresIdx}, ...
                'FontSize', PlotProps.Text.TitleSize, 'FontName', PlotProps.Text.FontName, ...
                'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
        end
    end

end