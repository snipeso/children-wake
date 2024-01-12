function events_el = fieldtrip2eeglab_events(events_ft)
% renames fields in the structure

Fields = fieldnames(events_ft);

Fields{strcmp(Fields, 'type')} = 'oldtype';
Fields{strcmp(Fields, 'value')} = 'type';
Fields{strcmp(Fields, 'sample')} = 'latency';


events_el = cell2struct(struct2cell(events_ft), Fields); % little hack