function String = disp_stats_descriptive(Data, String, Unit, Roundedness)
% String = disp_stats_descriptive(Data, String, Unit, Roundedness)
% Roundedness is precision of numbers ('%.0f')
% Data is P x 1 matrix.

if isnumeric(Roundedness)
    Roundedness = ['%.', num2str(Roundedness), 'f'];
end

IQ = quantile(Data, [.25 .75]);

if isempty(String)
String = [ num2str(mean(Data, 'omitnan'), Roundedness), Unit, ' [', ...
    num2str(IQ(1), Roundedness), ', ',  ...
    num2str(IQ(2), Roundedness), ']'];
else
String = [String, ' (N=', num2str(nnz(~isnan(Data))), '; MEAN, [Q1, Q3]): ',  ...
    num2str(mean(Data, 'omitnan'), Roundedness), Unit, ' [', ...
    num2str(IQ(1), Roundedness), ', ',  ...
    num2str(IQ(2), Roundedness), ']'];
disp(String)
end


