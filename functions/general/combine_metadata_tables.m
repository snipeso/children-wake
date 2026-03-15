function CombinedTable = combine_metadata_tables(Table1, Table2, UniqueIdentifierColumns)


CombinedTable = innerjoin(Table1, Table2, 'Keys', UniqueIdentifierColumns);