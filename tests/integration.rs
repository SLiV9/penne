//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use assert_cmd::Command;

#[test]
fn run_addition()
{
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("run");
	cmd.arg("examples/addition.pn");
	cmd.assert().success();
}

#[test]
fn build_addition()
{
	let outdir = tempfile::tempdir().unwrap();
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("--out-dir");
	cmd.arg(outdir.path());
	cmd.arg("examples/addition.pn");
	cmd.assert().success();
}

#[test]
fn emit_addition()
{
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("emit");
	cmd.arg("examples/addition.pn");
	cmd.assert().success();
}

#[test]
fn run_import_core()
{
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("run");
	cmd.arg("examples/import_core.pn");
	cmd.arg("core:text");
	cmd.assert().success();
}

#[test]
fn emit_wasm4_hello_from_penne()
{
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("emit");
	cmd.arg("--wasm");
	cmd.arg("examples/wasm4/hello_from_penne.pn");
	cmd.arg("vendor:wasm4");
	cmd.assert().success();
}

#[test]
fn emit_wasm4_write_with_custom_font()
{
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("emit");
	cmd.arg("--wasm");
	cmd.arg("examples/wasm4/write_with_custom_font.pn");
	cmd.arg("vendor:wasm4");
	cmd.assert().success();
}

#[test]
fn fail_to_build_loop_in_naked_branch()
{
	let outdir = tempfile::tempdir().unwrap();
	let mut cmd = Command::cargo_bin("penne").unwrap();
	cmd.arg("--out-dir");
	cmd.arg(outdir.path());
	cmd.arg("tests/samples/invalid/loop_in_naked_branch.pn");
	cmd.assert().failure();
}
