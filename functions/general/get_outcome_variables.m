function OutcomeVariables = get_outcome_variables(Metadata)
% Returns the manuscript outcome-variable names from a metadata table.

NotOutcomeVariables = {'Dataset', 'Participant', 'Session', 'SessionUnique', 'Sex', 'Handedness', ...
    'Age', 'Group', 'Subgroup', 'Condition', 'Hour', 'Task', 'Index', 'AgeGroups', 'EquispacedAges'};

AllVariables = Metadata.Properties.VariableNames;
OutcomeVariables = setdiff(AllVariables, NotOutcomeVariables);