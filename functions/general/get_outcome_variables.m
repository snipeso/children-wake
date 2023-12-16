function OutcomeVariables = get_outcome_variables(Metadata)

NotOutcomeVariables = {'Dataset', 'Participant', 'Session', 'Sex', 'Handedness', ...
    'Age', 'Group', 'Subgroup', 'Condition', 'Hour', 'Task', 'Index'};

AllVariables = Metadata.Properties.VariableNames;
OutcomeVariables = setdiff(AllVariables, NotOutcomeVariables);