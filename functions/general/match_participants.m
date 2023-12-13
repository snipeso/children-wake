function [MetadataPatients] = match_participants(MetadataPatients, MetadataControls)
% matches participants by gender and age (should be unique Patients)

MaxAgeGap = 1; % in years

nPatients = size(MetadataPatients, 1);
MetadataPatients.ControlParticipant = cell([nPatients, 1]);

MetadataPatients = sortrows(MetadataPatients, 'Age', 'ascend');
MetadataControls = sortrows(MetadataControls, 'Age', 'ascend');


for PatientIdx = 1:nPatients
    % match sex and task
    Controls = MetadataControls(strcmp(MetadataControls.Sex, MetadataPatients.Sex{PatientIdx}) & ...
        strcmp(MetadataControls.Task, MetadataPatients.Task{PatientIdx}), :);
    PatientAge = MetadataPatients.Age(PatientIdx);

    Index = dsearchn(Controls.Age, PatientAge);
    ControlAge = Controls.Age(Index);

    if abs(PatientAge-ControlAge) > MaxAgeGap
        warning(['couldnt find control for ', MetadataPatients.Participant{PatientIdx}])
          MetadataPatients.ControlParticipant(PatientIdx) = {'none'};
        continue
    end

    % add info to table
    MetadataPatients.ControlParticipant(PatientIdx) = Controls.Participant(Index);

    % delete row from controls
    MetadataControls(strcmp(MetadataControls.Participant, Controls.Participant{Index}), :) = [];
end

MetadataPatients(strcmp(MetadataPatients.ControlParticipant, 'none'), :) = [];