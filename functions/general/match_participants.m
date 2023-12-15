function [MetadataPatients, MetadataControls] = match_participants(Metadata, PatientIndexes)
% matches participants by gender, task and age (should be unique Patients)

MaxAgeGap = 1; % in years


MetadataPatients = Metadata(PatientIndexes, :);
MetadataControls = Metadata(~PatientIndexes, :);

UniquePatients = unique(MetadataPatients.Participant);
MetadataPatients.ControlParticipant = cell([size(MetadataPatients, 1), 1]);

MetadataPatients = sortrows(MetadataPatients, 'Age', 'ascend');
MetadataControls = sortrows(MetadataControls, 'Age', 'ascend');


for PatientIdx = 1:numel(UniquePatients)
    
    FirstPatientIdx = find(strcmp(MetadataPatients.Participant, UniquePatients{PatientIdx}), 1, 'first');
    AllPatientIndexes = contains(MetadataPatients.Participant, MetadataPatients.Participant(FirstPatientIdx));

    % match sex and task
    Controls = MetadataControls(strcmp(MetadataControls.Sex, MetadataPatients.Sex{FirstPatientIdx}) & ...
        strcmp(MetadataControls.Task, MetadataPatients.Task{FirstPatientIdx}), :);
    PatientAge = MetadataPatients.Age(FirstPatientIdx);

    ControlIndex = dsearchn(Controls.Age, PatientAge);
    ControlAge = Controls.Age(ControlIndex);

    if abs(PatientAge-ControlAge) > MaxAgeGap
        warning(['couldnt find control for ', MetadataPatients.Participant{FirstPatientIdx}])
          MetadataPatients.ControlParticipant(AllPatientIndexes) = repmat({'none'}, nnz(AllPatientIndexes), 1);
        continue
    end

    % add info to table
    MetadataPatients.ControlParticipant(AllPatientIndexes) = Controls.Participant(ControlIndex);

    % delete row from controls
    MetadataControls(strcmp(MetadataControls.Participant, Controls.Participant{ControlIndex}), :) = [];
end

MetadataPatients(strcmp(MetadataPatients.ControlParticipant, 'none'), :) = [];
MetadataPatients = sortrows(MetadataPatients, 'Participant');
MetadataControls = Metadata(contains(Metadata.Participant, MetadataPatients.ControlParticipant), :);
MetadataControls = sortrows(MetadataControls, 'Participant');


