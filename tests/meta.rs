//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::rebuilder::{Indentation, Rebuildable};

use comfy_table::Table;

type ValueType = penne::value_type::ValueType<&'static str>;

fn sample_types() -> impl std::iter::Iterator<Item = ValueType>
{
	sample_types_0()
		.chain(sample_types_rec(sample_types_0()))
		.chain(sample_types_rec(sample_types_rec(sample_types_0())))
}

fn sample_types_0() -> impl std::iter::Iterator<Item = ValueType>
{
	[
		ValueType::Void,
		ValueType::Int8,
		ValueType::Int64,
		ValueType::Int128,
		ValueType::Uint8,
		ValueType::Uint64,
		ValueType::Uint128,
		ValueType::Usize,
		ValueType::Char8,
		ValueType::Bool,
		ValueType::Struct {
			identifier: "State",
		},
		ValueType::Word {
			identifier: "Wd8",
			size_in_bytes: 1,
		},
		ValueType::Word {
			identifier: "Wd64",
			size_in_bytes: 8,
		},
		ValueType::Word {
			identifier: "Wd128",
			size_in_bytes: 16,
		},
	]
	.into_iter()
}

fn sample_types_rec(
	base: impl std::iter::Iterator<Item = ValueType>,
) -> impl std::iter::Iterator<Item = ValueType>
{
	base.flat_map(|vt| {
		let inner = Box::new(vt);
		[
			ValueType::Array {
				element_type: inner.clone(),
				length: 10,
			},
			ValueType::ArrayWithNamedLength {
				element_type: inner.clone(),
				named_length: "N",
			},
			ValueType::Slice {
				element_type: inner.clone(),
			},
			ValueType::SlicePointer {
				element_type: inner.clone(),
			},
			ValueType::EndlessArray {
				element_type: inner.clone(),
			},
			ValueType::Arraylike {
				element_type: inner.clone(),
			},
			ValueType::Pointer {
				deref_type: inner.clone(),
			},
			ValueType::View { deref_type: inner },
		]
		.into_iter()
	})
}

fn show_type(value_type: &ValueType) -> String
{
	const NO_INDENTATION: Indentation = Indentation {
		value: "",
		amount: 0,
	};
	value_type.rebuild(&NO_INDENTATION).unwrap()
}

fn show_type_if_neq(value_type: &ValueType, other_type: &ValueType) -> String
{
	if value_type == other_type
	{
		String::new()
	}
	else
	{
		show_type(value_type)
	}
}

fn num_if(value: Option<usize>) -> String
{
	match value
	{
		Some(num) => num.to_string(),
		None => String::new(),
	}
}

fn checkmark(value: bool) -> &'static str
{
	match value
	{
		true => "✓",
		false => "",
	}
}

#[test]
fn check_value_type_properties()
{
	let value_types = sample_types();

	let mut table = Table::new();
	table
		.load_preset(comfy_table::presets::UTF8_FULL)
		.apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS)
		.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);

	let mut illformeds = Table::new();
	illformeds
		.load_preset(comfy_table::presets::UTF8_FULL)
		.apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS)
		.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);
	let illformed_headers = ["Illformed types", "", "", "", "", "", "", ""];
	illformeds.set_header(illformed_headers);
	let mut illformed_row_number = 0;
	let mut illformed_row_len = 0;

	table.set_header([
		"Type",
		"s\ni\nz\ne\nd",
		"S",
		"k\nn\no\nw\nn",
		"W",
		"c\no\nn\ns\nt",
		"v\na\nr",
		"p\na\nr\na\nm",
		"r\ne\nt",
		"pd",
		"deref",
		"Type",
	]);
	for value_type in value_types
	{
		let debug_string = format!("{value_type:?}");
		if value_type.is_wellformed()
		{
			table.add_row([
				&show_type(&value_type),
				checkmark(value_type.can_be_sized()),
				checkmark(value_type.can_be_struct_member()),
				&num_if(value_type.known_size_in_bytes_as_word_member()),
				checkmark(value_type.can_be_word_member()),
				checkmark(value_type.can_be_constant()),
				checkmark(value_type.can_be_variable()),
				checkmark(value_type.can_be_parameter()),
				checkmark(value_type.can_be_returned()),
				&"&".repeat(value_type.pointer_depth()),
				&show_type_if_neq(
					&value_type.fully_dereferenced(),
					&value_type,
				),
				&debug_string,
			]);
		}
		else
		{
			if illformed_row_len == 0
			{
				illformeds.add_row([show_type(&value_type)]);
				illformed_row_len = 1;
			}
			else if illformed_row_len == illformed_headers.len()
			{
				illformed_row_number += 1;
				illformeds.add_row([show_type(&value_type)]);
				illformed_row_len = 1;
			}
			else
			{
				illformeds
					.row_mut(illformed_row_number)
					.unwrap()
					.add_cell(show_type(&value_type).into());
				illformed_row_len += 1;
			}
		}
	}
	println!("{table}");
	println!("{illformeds}");
}
