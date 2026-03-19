function events_el = fieldtrip2eeglab_events(events_ft)
% Renames FieldTrip event fields into EEGLAB-style event structs.

Fields = fieldnames(events_ft);

Fields{strcmp(Fields, 'type')} = 'oldtype';
Fields{strcmp(Fields, 'value')} = 'type';
Fields{strcmp(Fields, 'sample')} = 'latency';


events_el = cell2struct(struct2cell(events_ft), Fields); % little hack
