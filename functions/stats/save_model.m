function save_model(Model, Filename)
% Writes a mixed-model summary to disk.

Text = formattedDisplayText(Model);

Text = replace(Text, '<strong>', '');
Text = replace(Text, '</strong>', '');

fid = fopen(Filename, 'wt'); % a full path would be better
fprintf(fid,'%s\n', Text); 
fclose(fid); 