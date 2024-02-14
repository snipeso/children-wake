function disp_mixed_stat(Model, Coefficient)

RowIdx = strcmp(Model.Coefficients.Name, Coefficient);

PValue = Model.Coefficients.pValue(RowIdx);
Estimate = Model.Coefficients.Estimate(RowIdx);
tStat = Model.Coefficients.tStat(RowIdx);

DF = Model.Coefficients.DF(RowIdx);

pString = extractAfter(num2str(PValue, '%.3f'), '.');
if PValue < .001
    pString = ', p < .001';
else
    pString = [', p = .',pString];
end

disp([Coefficient, ': beta = ' num2str(Estimate, '%.3f'), ...
    ', t = ', num2str(tStat, '%.2f'), pString, ', df = ', num2str(DF)])
