SELECT unquoted_field, "my_field", 'string', B'010101',
-- Numbers
	5, 5.0, 5.0_0, 5_000,
-- Type casts
	bool 'yes', 'yes'::bool, CAST ('yes' AS bool),
-- Positional params
	$1, $37,
-- Parenths
	(1), (('a string')),
-- correlation
	tbl.col, "mytbl".other,
-- indexing
	arr[0], arr[start:stop], mytable.two_d_column[17][34],
-- field expressions,
	tbl.col2, tbl.*,

'the end';
