function CorrectedMetadata = correct_for_age(Metadata)
% identifies the linear correlation between each outcome variable and age,
% and adjusts the outcome value accordingly

OutcomeVariables = get_outcome_variables(Metadata);
CorrectedMetadata = Metadata;

x = Metadata.Age;

for Variable = OutcomeVariables

    y = Metadata.(Variable{1});
    try
    p = polyfit(x,y,1);
    catch
        a=1
    end
    y1 = polyval(p,x);

    CorrectedMetadata.(Variable{1}) = y-y1;
end
