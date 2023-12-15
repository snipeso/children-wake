function Struct = average_field(Struct, Fieldname)

NewFieldname = [Fieldname, 'Average'];
for Indx = 1:numel(Struct)
    Struct(Indx).(NewFieldname) =  mean(Struct(Indx).(Fieldname), 'omitnan');

end

