


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
PlotProps.Stats.PlotN = true;

figure('Units','normalized','Position', [0 0 .35 .15*nMeasures])
for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    % overnight changes
    for HourIdx = 1:numel(Hours)
        
        HourMetadata = TempMetadata(strcmp(TempMetadata.Hour, Hours{HourIdx}), :);
        
        [MetadataPatients, MetadataControls] = match_participants(HourMetadata, strcmp(HourMetadata.Group, 'ADHD'));

        ADHD = Topographies(MetadataPatients.Index, :);
        Control = Topographies(MetadataControls.Index, :);

        ADHD = average_by_column(MetadataPatients, MetadataPatients.Index, ADHD, 'Participant');
        Control = average_by_column(MetadataControls, MetadataControls.Index, Control, 'Participant');

        chART.sub_plot([], [nMeasures, 2], [MeasuresIdx, HourIdx], [], false, '', PlotProps);
        StatsParameters = Parameters.Stats;
        StatsParameters.Unpaired = true;
        plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
        colorbar off


        X = get(gca, 'XLim');
        Y = get(gca, 'YLim');
        if HourIdx ==1
            text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasuresIdx}, ...
                'FontSize', PlotProps.Text.TitleSize, 'FontName', PlotProps.Text.FontName, ...
                'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
        end
    end

end