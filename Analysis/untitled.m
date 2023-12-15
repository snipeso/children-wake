


% paired t-tests across channels for ADHD and controls
% Ages = [7.5 14];

Measures = fieldnames(BurstInformationTopography);

nMeasures = numel(Measures);

% TempMetadata = Metadata(Metadata.Age >=Ages(1) & Metadata.Age<=Ages(2), :);
TempMetadata = Metadata;
TempMetadata.GroupAge = discretize(TempMetadata.Age, [8 15 25]);


OvernightMetadata = overnight_changes(TempMetadata);
% [OvernightMetadataPatients, OvernightMetadataControls] = match_participants(OvernightMetadata, strcmp(OvernightMetadata.Group, 'ADHD'));
OvernightMetadataPatients = OvernightMetadata(OvernightMetadata.GroupAge ==1, :);
OvernightMetadataControls = OvernightMetadata(OvernightMetadata.GroupAge ==2, :);

Groups = {'HC', 'ADHD'};
HourLabels = {'Evening', 'Morning'};
CLims = [-1 1];
PlotProps.Stats.PlotN = true;
StatsParameters = Parameters.Stats;
StatsParameters.Unpaired = true;

figure('Units','normalized','Position', [0 0 .35 .15*nMeasures])
for MeasuresIdx = 1:nMeasures

    Topographies = BurstInformationTopography.(Measures{MeasuresIdx});

    % group differences at each hour
    for HourIdx = 1:numel(Hours)

        HourMetadata = TempMetadata(strcmp(TempMetadata.Hour, Hours{HourIdx}), :);

        % [MetadataPatients, MetadataControls] = match_participants(HourMetadata, strcmp(HourMetadata.Group, 'ADHD'));
        MetadataPatients = HourMetadata(HourMetadata.GroupAge==1, :);
         MetadataControls = HourMetadata(HourMetadata.GroupAge==2, :);

        ADHD = Topographies(MetadataPatients.Index, :);
        Control = Topographies(MetadataControls.Index, :);

        ADHD = average_by_column(MetadataPatients, MetadataPatients.Index, ADHD, 'Participant');
        Control = average_by_column(MetadataControls, MetadataControls.Index, Control, 'Participant');

        chART.sub_plot([], [nMeasures, 3], [MeasuresIdx, HourIdx], [], false, '', PlotProps);
        plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
        colorbar off

        X = get(gca, 'XLim');
        Y = get(gca, 'YLim');
        if HourIdx ==1
            text(X(1)-diff(X)*.15, Y(1)+diff(Y)*.5, Measures{MeasuresIdx}, ...
                'FontSize', PlotProps.Text.TitleSize, 'FontName', PlotProps.Text.FontName, ...
                'FontWeight', 'Bold', 'HorizontalAlignment', 'Center', 'Rotation', 90);
        end
        if MeasuresIdx ==1
            title(HourLabels{HourIdx})
        end
    end


    % overnight differences patients and controls
    ChangeADHD = Topographies(OvernightMetadataPatients.MorningIndexes, :)-Topographies(OvernightMetadataPatients.EveningIndexes, :);
    ChangeControls = Topographies(OvernightMetadataControls.MorningIndexes, :)-Topographies(OvernightMetadataControls.EveningIndexes, :);


    ADHD = average_by_column(OvernightMetadataPatients, OvernightMetadataPatients.Index, ChangeADHD, 'Participant');
    Control = average_by_column(OvernightMetadataControls, OvernightMetadataControls.Index, ChangeControls, 'Participant');

    chART.sub_plot([], [nMeasures, 3], [MeasuresIdx, 3], [], false, '', PlotProps);
    plot_topography_difference(Control, ADHD, Chanlocs, CLims, StatsParameters, PlotProps)
    colorbar off
end