function CombinedTable = combine_metadata_tables(Table1, Table2, UniqueIdentifierColumns)
% Joins two metadata tables on shared identifiers such as participant and session.


CombinedTable = innerjoin(Table1, Table2, 'Keys', UniqueIdentifierColumns);