//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::test_suite::assert_formatted_correctly;

#[test]
fn rebuild_goto_end() -> Result<(), anyhow::Error>
{
	assert_formatted_correctly("examples/goto_end.pn")
}
