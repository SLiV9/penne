//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType
{
	Int8,
	Int16,
	Int32,
	Int64,
	Int128,
	Uint8,
	Uint16,
	Uint32,
	Uint64,
	Uint128,
	Usize,
	Bool,
	Char,
	String,
	Array
	{
		element_type: Box<ValueType>,
		length: usize,
	},
	Slice
	{
		element_type: Box<ValueType>,
	},
	ExtArray
	{
		element_type: Box<ValueType>,
	},
	Pointer
	{
		deref_type: Box<ValueType>,
	},
	View
	{
		deref_type: Box<ValueType>,
	},
}

impl ValueType
{
	pub fn for_byte_string() -> ValueType
	{
		ValueType::Slice {
			element_type: Box::new(ValueType::Uint8),
		}
	}

	pub fn is_integral(&self) -> bool
	{
		match self
		{
			ValueType::Int8 => true,
			ValueType::Int16 => true,
			ValueType::Int32 => true,
			ValueType::Int64 => true,
			ValueType::Int128 => true,
			ValueType::Uint8 => true,
			ValueType::Uint16 => true,
			ValueType::Uint32 => true,
			ValueType::Uint64 => true,
			ValueType::Uint128 => true,
			ValueType::Usize => true,
			ValueType::Bool => false,
			ValueType::Char => false,
			ValueType::String => false,
			ValueType::Array { .. } => false,
			ValueType::Slice { .. } => false,
			ValueType::ExtArray { .. } => false,
			ValueType::Pointer { .. } => false,
			ValueType::View { .. } => false,
		}
	}

	pub fn fixed_bit_length(&self) -> usize
	{
		match self
		{
			ValueType::Int8 => 8,
			ValueType::Int16 => 16,
			ValueType::Int32 => 32,
			ValueType::Int64 => 64,
			ValueType::Int128 => 128,
			ValueType::Uint8 => 8,
			ValueType::Uint16 => 16,
			ValueType::Uint32 => 32,
			ValueType::Uint64 => 64,
			ValueType::Uint128 => 128,
			_ => 0,
		}
	}

	pub fn is_signed(&self) -> bool
	{
		match self
		{
			ValueType::Int8 => true,
			ValueType::Int16 => true,
			ValueType::Int32 => true,
			ValueType::Int64 => true,
			ValueType::Int128 => true,
			_ => false,
		}
	}

	pub fn can_be_used_as(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				_ => self == other,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_be_declared_as(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a == b,
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_coerce_into(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a == b,
				ValueType::ExtArray { element_type: b } => a == b,
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } => a == b,
					_ => false,
				},
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::ExtArray { element_type: b } => a == b,
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } => a == b,
					_ => false,
				},
				_ => false,
			},
			_ => false,
		}
	}

	pub fn can_autoderef_into(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a.can_be_used_as(b),
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } =>
					{
						a.can_be_used_as(b)
					}
					_ => false,
				},
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } =>
					{
						a.can_be_used_as(b)
					}
					_ => false,
				},
				_ => false,
			},
			ValueType::View { deref_type } =>
			{
				deref_type.as_ref().can_be_used_as(other)
					|| deref_type.can_autoderef_into(other)
					|| other.get_viewee_type().map_or(false, |t| {
						deref_type.as_ref().can_be_used_as(&t)
							|| deref_type.can_autoderef_into(&t)
					})
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.as_ref().can_be_used_as(other)
					|| deref_type.can_autoderef_into(other)
					|| other.get_pointee_type().map_or(false, |t| {
						deref_type.as_ref().can_be_used_as(&t)
							|| deref_type.can_autoderef_into(&t)
					})
			}
			_ => false,
		}
	}

	pub fn get_element_type(&self) -> Option<ValueType>
	{
		match self
		{
			ValueType::Array {
				element_type,
				length: _,
			} => Some(element_type.as_ref().clone()),
			ValueType::Slice { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			ValueType::ExtArray { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			_ => None,
		}
	}

	pub fn get_pointee_type(&self) -> Option<ValueType>
	{
		match self
		{
			ValueType::Pointer { deref_type } =>
			{
				Some(deref_type.as_ref().clone())
			}
			_ => None,
		}
	}

	pub fn get_viewee_type(&self) -> Option<ValueType>
	{
		match self
		{
			ValueType::View { deref_type } => Some(deref_type.as_ref().clone()),
			_ => None,
		}
	}

	pub fn fully_dereferenced(&self) -> ValueType
	{
		match self
		{
			ValueType::Pointer { deref_type } =>
			{
				deref_type.fully_dereferenced()
			}
			ValueType::View { deref_type } => deref_type.fully_dereferenced(),
			_ => self.clone(),
		}
	}
}
