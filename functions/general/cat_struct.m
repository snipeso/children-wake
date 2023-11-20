function Struct = cat_struct(Struct1, Struct2)
% concatente structures, also when potentially empty.

if isempty(Struct1) || numel(fieldnames(Struct1)) == 0 
    Struct = Struct2;
elseif isempty(Struct2) || numel(fieldnames(Struct2)) == 0
    Struct = Struct1;
else
    Struct = cat(2, Struct1, Struct2);
end





