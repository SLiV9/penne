//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! Each variable, expression and return value has a [ValueType].

pub trait Identifier: Clone + PartialEq {}

impl Identifier for String {}

#[must_use]
#[derive(Debug, Clone, PartialEq)]
pub enum ValueType<I>
where
	I: Identifier,
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
		element_type: Box<ValueType<I>>,
		length: usize,
	},
	Slice
	{
		element_type: Box<ValueType<I>>,
	},
	EndlessArray
	{
		element_type: Box<ValueType<I>>,
	},
	Arraylike
	{
		element_type: Box<ValueType<I>>,
	},
	Struct
	{
		identifier: I,
		size_in_bytes: usize,
	},
	Word
	{
		identifier: I,
		size_in_bytes: usize,
	},
	UnresolvedStructOrWord
	{
		identifier: Option<I>,
	},
	Pointer
	{
		deref_type: Box<ValueType<I>>,
	},
	View
	{
		deref_type: Box<ValueType<I>>,
	},
}

#[derive(Debug, Clone)]
pub enum OperandValueType<I>
where
	I: Identifier,
{
	ValueType(ValueType<I>),
	Pointer,
}

impl<I> ValueType<I>
where
	I: Identifier,
{
	pub fn for_byte_string() -> ValueType<I>
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
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Struct { .. } => false,
			ValueType::Word { .. } => false,
			ValueType::UnresolvedStructOrWord { .. } => false,
			ValueType::Pointer { .. } => false,
			ValueType::View { .. } => false,
		}
	}

	pub fn known_aligned_size_in_bytes(&self) -> Option<usize>
	{
		match self
		{
			ValueType::Int8 => Some(1),
			ValueType::Int16 => Some(2),
			ValueType::Int32 => Some(4),
			ValueType::Int64 => Some(8),
			ValueType::Int128 => Some(16),
			ValueType::Uint8 => Some(1),
			ValueType::Uint16 => Some(2),
			ValueType::Uint32 => Some(4),
			ValueType::Uint64 => Some(8),
			ValueType::Uint128 => Some(16),
			ValueType::Usize =>
			{
				// TODO think about this again
				Some(8)
			}
			ValueType::Bool => Some(1),
			ValueType::Char => None,
			ValueType::String => None,
			ValueType::Array {
				element_type,
				length,
			} => element_type
				.known_aligned_size_in_bytes()
				.and_then(|x| Some(length * x)),
			ValueType::Slice { .. } => None,
			ValueType::EndlessArray { .. } => None,
			ValueType::Arraylike { .. } => None,
			ValueType::Struct { size_in_bytes, .. } => Some(*size_in_bytes),
			ValueType::Word { size_in_bytes, .. } => Some(*size_in_bytes),
			ValueType::UnresolvedStructOrWord { .. } => None,
			ValueType::Pointer { .. } =>
			{
				// TODO think about this again
				Some(8)
			}
			ValueType::View { .. } => None,
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
			ValueType::Word { size_in_bytes, .. } => 8 * size_in_bytes,
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

	pub fn can_be_used_as(&self, other: &ValueType<I>) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Arraylike { element_type: b } => a.can_be_used_as(b),
				_ => self == other,
			},
			ValueType::EndlessArray { element_type: a } => match other
			{
				ValueType::Arraylike { element_type: b } => a.can_be_used_as(b),
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_be_declared_as(&self, other: &ValueType<I>) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Arraylike { element_type: b } => a == b,
				_ => self == other,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::Arraylike { element_type: b } => a == b,
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_be_concretization_of(&self, other: &ValueType<I>) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: la,
			} => match other
			{
				ValueType::Array {
					element_type: b,
					length: lb,
				} => la == lb && a.can_be_concretization_of(b),
				_ => self.can_be_used_as(other),
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::Slice { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				ValueType::Arraylike { element_type: b } => a.can_be_used_as(b),
				_ => self.can_be_used_as(other),
			},
			ValueType::EndlessArray { element_type: a } => match other
			{
				ValueType::EndlessArray { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self.can_be_used_as(other),
			},
			ValueType::Arraylike { element_type: a } => match other
			{
				ValueType::Arraylike { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self.can_be_used_as(other),
			},
			ValueType::Struct { identifier: a, .. } => match other
			{
				ValueType::UnresolvedStructOrWord { identifier: None } => true,
				ValueType::UnresolvedStructOrWord {
					identifier: Some(b),
				} => a == b,
				_ => self == other,
			},
			ValueType::Word { identifier: a, .. } => match other
			{
				ValueType::UnresolvedStructOrWord { identifier: None } => true,
				ValueType::UnresolvedStructOrWord {
					identifier: Some(b),
				} => a == b,
				_ => self == other,
			},
			ValueType::View { deref_type: a } => match other
			{
				ValueType::View { deref_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self.can_be_used_as(other),
			},
			ValueType::Pointer { deref_type: a } => match other
			{
				ValueType::Pointer { deref_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self.can_be_used_as(other),
			},
			_ => self.can_be_used_as(other),
		}
	}

	pub fn can_coerce_into(&self, other: &ValueType<I>) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a == b,
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a == b,
					_ => false,
				},
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a == b,
					_ => false,
				},
				_ => false,
			},
			_ => false,
		}
	}

	pub fn can_coerce_address_into_pointer_to(
		&self,
		other: &ValueType<I>,
	) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a == b,
				ValueType::EndlessArray { element_type: b } => a == b,
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::EndlessArray { element_type: b } => a == b,
				_ => false,
			},
			_ => false,
		}
	}

	pub fn can_autoderef_into(&self, other: &ValueType<I>) -> bool
	{
		match self
		{
			ValueType::Array { .. } =>
			{
				self == other || self.can_coerce_into(other)
			}
			ValueType::Slice { .. } =>
			{
				self == other || self.can_coerce_into(other)
			}
			ValueType::EndlessArray { .. } =>
			{
				self == other || self.can_coerce_into(other)
			}
			ValueType::View { deref_type } =>
			{
				deref_type.as_ref() == other
					|| deref_type.can_subautoderef_into(other)
					|| other.get_viewee_type().map_or(false, |t| {
						deref_type.as_ref() == &t
							|| deref_type.can_subautoderef_into(&t)
					})
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.as_ref() == other
					|| deref_type.can_subautoderef_into(other)
					|| other.get_pointee_type().map_or(false, |t| {
						deref_type.as_ref() == &t
							|| deref_type.can_subautoderef_into(&t)
							|| deref_type.can_coerce_address_into_pointer_to(&t)
					})
			}
			_ => false,
		}
	}

	fn can_subautoderef_into(&self, other: &ValueType<I>) -> bool
	{
		match self
		{
			ValueType::Array { .. } => self == other,
			ValueType::Slice { .. } => self == other,
			ValueType::EndlessArray { .. } => self == other,
			ValueType::View { deref_type } =>
			{
				deref_type.as_ref() == other
					|| deref_type.can_subautoderef_into(other)
					|| other.get_viewee_type().map_or(false, |t| {
						deref_type.as_ref() == &t
							|| deref_type.can_subautoderef_into(&t)
					})
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.as_ref() == other
					|| deref_type.can_subautoderef_into(other)
					|| other.get_pointee_type().map_or(false, |t| {
						deref_type.as_ref() == &t
							|| deref_type.can_subautoderef_into(&t)
					})
			}
			_ => false,
		}
	}

	pub fn is_wellformed(&self) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type,
				length,
			} => *length > 0 && element_type.is_wellformed_element(),
			ValueType::Slice { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::EndlessArray { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::Arraylike { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.is_wellformed_inner()
			}
			ValueType::View { deref_type } => deref_type.is_wellformed_inner(),
			_ => true,
		}
	}

	fn is_wellformed_element(&self) -> bool
	{
		match self
		{
			ValueType::Array { .. } => self.is_wellformed_inner(),
			ValueType::Slice { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Pointer { .. } => self.is_wellformed_inner(),
			ValueType::View { .. } => false,
			_ => self.is_wellformed_inner(),
		}
	}

	fn is_wellformed_inner(&self) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type,
				length,
			} => *length > 0 && element_type.is_wellformed_element(),
			ValueType::Slice { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::EndlessArray { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::Arraylike { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.is_wellformed_inner()
			}
			ValueType::View { .. } => false,
			_ => true,
		}
	}

	pub fn can_be_struct_member(&self) -> bool
	{
		match self
		{
			ValueType::Slice { .. } =>
			{
				// Maybe yes?
				false
			}
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::View { .. } =>
			{
				// Maybe yes?
				false
			}
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_word_member(&self) -> bool
	{
		match self
		{
			ValueType::Array { .. } => false,
			ValueType::Slice { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Struct { .. } => false,
			ValueType::View { .. } => false,
			ValueType::Pointer { .. } => false,
			ValueType::Usize { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_constant(&self) -> bool
	{
		match self
		{
			ValueType::Slice { .. } =>
			{
				// Maybe yes?
				false
			}
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_variable(&self) -> bool
	{
		match self
		{
			ValueType::Slice { .. } =>
			{
				// Maybe yes?
				false
			}
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::View { .. } =>
			{
				// Maybe yes?
				false
			}
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_parameter(&self) -> bool
	{
		match self
		{
			ValueType::Array { .. } => false,
			ValueType::Struct { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_returned(&self) -> bool
	{
		match self
		{
			ValueType::Array { .. } => false,
			ValueType::Slice { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Struct { .. } => false,
			ValueType::Word { .. } => true,
			ValueType::UnresolvedStructOrWord { .. } => false,
			ValueType::View { .. } =>
			{
				// Maybe yes?
				false
			}
			_ => self.is_wellformed(),
		}
	}

	pub fn get_element_type(&self) -> Option<ValueType<I>>
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
			ValueType::EndlessArray { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			ValueType::Arraylike { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			_ => None,
		}
	}

	pub fn get_pointee_type(&self) -> Option<ValueType<I>>
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

	pub fn get_viewee_type(&self) -> Option<ValueType<I>>
	{
		match self
		{
			ValueType::View { deref_type } => Some(deref_type.as_ref().clone()),
			_ => None,
		}
	}

	pub fn fully_dereferenced(&self) -> ValueType<I>
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
