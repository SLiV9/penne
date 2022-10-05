//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub use crate::lexer::Location;

#[derive(Debug, Clone)]
pub enum Poisonable<T>
{
	Some(T),
	Poison(Poison<T>),
	None,
}

#[derive(Debug, Clone)]
pub enum Poison<T>
{
	Error
	{
		error: Error,
		partial: Option<T>,
	},
	Poisoned,
}

#[derive(Debug, Clone)]
pub enum Error
{
	Foo,
}
