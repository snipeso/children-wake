function [MetadataPatients, MetadataControls] = match_participants(Metadata, PatientIndexes)
% matches participants by gender, task and age (should be unique Patients)

MaxAgeGap = 1; % in years


MetadataPatients = Metadata(PatientIndexes, :);
MetadataControls = Metadata(~PatientIndexes, :);

[UniquePatients, Indexes] = unique(MetadataPatients.Participant);
[~, Order] = sort(MetadataPatients.Subgroup(Indexes));
UniquePatients = UniquePatients(Order);
Indexes = Indexes(Order);

MetadataPatients.ControlParticipant = cell([size(MetadataPatients, 1), 1]);

Unmatched = table();
for FirstPatientIdx = Indexes'

    AllPatientIndexes = contains(MetadataPatients.Participant, MetadataPatients.Participant(FirstPatientIdx));

    % match sex and task
    % Controls = MetadataControls(strcmp(MetadataControls.Sex, MetadataPatients.Sex{FirstPatientIdx}) & ...
    %     strcmp(MetadataControls.Task, MetadataPatients.Task{FirstPatientIdx}), :);
    Controls = MetadataControls(strcmp(MetadataControls.Sex, MetadataPatients.Sex{FirstPatientIdx}), :);
    PatientAge = MetadataPatients.Age(FirstPatientIdx);

    if isempty(Controls)
        MetadataPatients.ControlParticipant(AllPatientIndexes) = repmat({'none'}, nnz(AllPatientIndexes), 1);
        warning(['cant match', MetadataPatients.Participant{FirstPatientIdx}])
        Unmatched = cat(1, Unmatched, MetadataPatients(FirstPatientIdx, :));
        continue
    end

    ControlIndex = dsearchn(Controls.Age, PatientAge);

    ControlAge = Controls.Age(ControlIndex);

    if abs(PatientAge-ControlAge) > MaxAgeGap
        MetadataPatients.ControlParticipant(AllPatientIndexes) = repmat({'none'}, nnz(AllPatientIndexes), 1);
        warning(['cant match', MetadataPatients.Participant{FirstPatientIdx}])
        Unmatched = cat(1, Unmatched, MetadataPatients(FirstPatientIdx, :));
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


clc

UniquePatientMetadata = unique_metadata(MetadataPatients);
UniqueControlMetadata = unique_metadata(MetadataControls);
disp(['Final N: ', num2str(size(UniquePatientMetadata, 1))])

% n female per group

nADHDm = nnz(contains(UniquePatientMetadata.Sex, 'm'));
nHCm =  nnz(contains(UniqueControlMetadata.Sex, 'm'));
disp(['ADHD m: ' num2str(nADHDm), '; HC m: ', num2str(nHCm)])

% average age per group
disp(['ADHD age: ' num2str( mean(UniquePatientMetadata.Age)), '; HC age: ', num2str( mean(UniqueControlMetadata.Age))])

