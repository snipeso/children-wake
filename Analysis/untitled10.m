close all

Groups = {'HC', 'ADHD'};
Tasks = {'Oddball', 'Alertness'};
YVariables = {'Globality', 'Amplitude', 'Duration', 'Frequency'};
Session = 'Session_1';
Colors = chART.color_picker(2);
Markers = {'o', '^'};

OvernightMetadata = 1;

for Variable = YVariables
    figure
    hold on
    for GroupIdx = 1:numel(Groups)
        for TaskIdx = 1:numel(Tasks)

            Indexes = strcmp(OvernightMetadata.Group, Groups{GroupIdx}) & contains(OvernightMetadata.Task, Tasks{TaskIdx}) & ...
                strcmp(OvernightMetadata.Hour, Hour) & contains(OvernightMetadata.Session, Session);
            scatter(OvernightMetadata.Age(Indexes), OvernightMetadata.(Variable{1})(Indexes), 10, Markers{TaskIdx}, ...
                'MarkerEdgeColor','none', 'MarkerFaceColor', Colors(GroupIdx,:))
            lsline
            xlabel('Age')
            ylabel(['\Delta', Variable{1}])
            title('Overnight change')
        end
    end
    chART.save_figure(['OvernightScatterAge', Session,Variable{1}], ResultsFolder, PlotProps)
end
