//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! Each variable, expression and return value has a [ValueType].

pub trait Identifier: Clone + PartialEq {}

impl Identifier for String {}

impl Identifier for &'static str {}

/// The maximum alignment of structure members is 8 bytes.
pub const MAXIMUM_ALIGNMENT: usize = 8;

#[must_use]
#[derive(Debug, Clone, PartialEq)]
pub enum ValueType<I>
where
	I: Identifier,
{
	Void,
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
	Char8,
	Bool,
	Array
	{
		element_type: Box<Self>,
		length: usize,
	},
	ArrayWithNamedLength
	{
		element_type: Box<Self>,
		named_length: I,
	},
	Slice
	{
		element_type: Box<Self>,
	},
	SlicePointer
	{
		element_type: Box<Self>,
	},
	EndlessArray
	{
		element_type: Box<Self>,
	},
	Arraylike
	{
		element_type: Box<Self>,
	},
	Struct
	{
		identifier: I,
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
		deref_type: Box<Self>,
	},
	View
	{
		deref_type: Box<Self>,
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
	pub fn is_void(&self) -> bool
	{
		match self
		{
			ValueType::Void => true,
			_ => false,
		}
	}

	pub fn is_integral(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
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
			ValueType::Char8 => false,
			ValueType::Bool => false,
			ValueType::Array { .. } => false,
			ValueType::ArrayWithNamedLength { .. } => false,
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Struct { .. } => false,
			ValueType::Word { .. } => false,
			ValueType::UnresolvedStructOrWord { .. } => false,
			ValueType::Pointer { .. } => false,
			ValueType::View { .. } => false,
		}
	}

	pub fn known_size_in_bytes_as_word_member(&self) -> Option<usize>
	{
		match self
		{
			ValueType::Void => None,
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
			ValueType::Usize => None,
			ValueType::Char8 => Some(1),
			ValueType::Bool => Some(1),
			ValueType::Array { .. } => None,
			ValueType::ArrayWithNamedLength { .. } => None,
			ValueType::Slice { .. } => None,
			ValueType::SlicePointer { .. } => None,
			ValueType::EndlessArray { .. } => None,
			ValueType::Arraylike { .. } => None,
			ValueType::Struct { .. } => None,
			ValueType::Word { size_in_bytes, .. } => Some(*size_in_bytes),
			ValueType::UnresolvedStructOrWord { .. } => None,
			ValueType::Pointer { .. } => None,
			ValueType::View { .. } => None,
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

	pub fn is_bitfield(&self) -> bool
	{
		match self
		{
			ValueType::Uint8 => true,
			ValueType::Uint16 => true,
			ValueType::Uint32 => true,
			ValueType::Uint64 => true,
			ValueType::Uint128 => true,
			_ => false,
		}
	}

	fn is_alias_of(&self, other: &Self) -> bool
	{
		match (self, other)
		{
			(ValueType::Char8, ValueType::Uint8) => true,
			_ => false,
		}
	}

	fn equals(&self, other: &Self) -> bool
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
				} => la == lb && a.equals(b),
				_ => false,
			},
			ValueType::ArrayWithNamedLength {
				element_type: a,
				named_length: x,
			} => match other
			{
				ValueType::ArrayWithNamedLength {
					element_type: b,
					named_length: y,
				} => x == y && a.equals(b),
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::Slice { element_type: b } => a.equals(b),
				_ => false,
			},
			ValueType::SlicePointer { element_type: a } => match other
			{
				ValueType::SlicePointer { element_type: b } => a.equals(b),
				_ => false,
			},
			ValueType::EndlessArray { element_type: a } => match other
			{
				ValueType::EndlessArray { element_type: b } => a.equals(b),
				_ => false,
			},
			ValueType::Arraylike { element_type: a } => match other
			{
				ValueType::Arraylike { element_type: b } => a.equals(b),
				_ => false,
			},
			ValueType::Struct { .. } => self == other,
			ValueType::Word { .. } => self == other,
			ValueType::View { deref_type: a } => match other
			{
				ValueType::View { deref_type: b } => a.equals(b),
				_ => false,
			},
			ValueType::Pointer { deref_type: a } => match other
			{
				ValueType::Pointer { deref_type: b } => a.equals(b),
				_ => false,
			},
			_ =>
			{
				self == other
					|| self.is_alias_of(other)
					|| other.is_alias_of(self)
			}
		}
	}

	fn is_like(&self, other: &Self) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Arraylike { element_type: b } => a.is_like(b),
				_ => self == other,
			},
			ValueType::ArrayWithNamedLength {
				element_type: a,
				named_length: _,
			} => match other
			{
				ValueType::Arraylike { element_type: b } => a.is_like(b),
				_ => self == other,
			},
			ValueType::EndlessArray { element_type: a } => match other
			{
				ValueType::Arraylike { element_type: b } => a.is_like(b),
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_be_declared_as(&self, other: &Self) -> bool
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
			ValueType::ArrayWithNamedLength {
				element_type: a,
				named_length: _,
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
			ValueType::SlicePointer { element_type: a } => match other
			{
				ValueType::Pointer { deref_type } => match deref_type.as_ref()
				{
					ValueType::Arraylike { element_type: b } => a == b,
					_ => false,
				},
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_be_concretization_of(&self, other: &Self) -> bool
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
				_ => self.is_like(other),
			},
			ValueType::ArrayWithNamedLength {
				element_type: a,
				named_length: x,
			} => match other
			{
				ValueType::ArrayWithNamedLength {
					element_type: b,
					named_length: y,
				} => x == y && a.can_be_concretization_of(b),
				_ => self.is_like(other),
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::Slice { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				ValueType::Arraylike { element_type: b } => a.is_like(b),
				_ => self == other,
			},
			ValueType::SlicePointer { element_type: a } => match other
			{
				ValueType::SlicePointer { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				ValueType::Arraylike { element_type: b } => a.is_like(b),
				ValueType::Pointer { deref_type } => match deref_type.as_ref()
				{
					ValueType::Arraylike { element_type: b } => a.is_like(b),
					_ => self == other,
				},
				_ => self == other,
			},
			ValueType::EndlessArray { element_type: a } => match other
			{
				ValueType::EndlessArray { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self.is_like(other),
			},
			ValueType::Arraylike { element_type: a } => match other
			{
				ValueType::Arraylike { element_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self == other,
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
				_ => self == other,
			},
			ValueType::Pointer { deref_type: a } => match other
			{
				ValueType::Pointer { deref_type: b } =>
				{
					a.can_be_concretization_of(b)
				}
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_coerce_into(&self, other: &Self) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a.equals(b),
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a.equals(b),
					_ => false,
				},
				_ => false,
			},
			ValueType::ArrayWithNamedLength {
				element_type: a,
				named_length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a.equals(b),
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a.equals(b),
					_ => false,
				},
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a.equals(b),
					_ => false,
				},
				_ => false,
			},
			ValueType::SlicePointer { element_type: a } => match other
			{
				ValueType::Pointer { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a.equals(b),
					_ => false,
				},
				_ => false,
			},
			ValueType::Struct { .. } => match other
			{
				ValueType::View { deref_type } => deref_type.as_ref() == self,
				_ => false,
			},
			_ => false,
		}
	}

	pub fn can_coerce_address_into(&self, other: &Self) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::SlicePointer { element_type: b } => a.equals(b),
				ValueType::Pointer { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a.equals(b),
					_ => false,
				},
				_ => false,
			},
			ValueType::ArrayWithNamedLength {
				element_type: a,
				named_length: _,
			} => match other
			{
				ValueType::SlicePointer { element_type: b } => a.equals(b),
				ValueType::Pointer { deref_type } => match deref_type.as_ref()
				{
					ValueType::EndlessArray { element_type: b } => a.equals(b),
					_ => false,
				},
				_ => false,
			},
			_ => false,
		}
	}

	pub fn can_autoderef_into(&self, other: &Self) -> bool
	{
		match self
		{
			ValueType::Array { .. } =>
			{
				self.equals(other) || self.can_coerce_into(other)
			}
			ValueType::ArrayWithNamedLength { .. } =>
			{
				self.equals(other) || self.can_coerce_into(other)
			}
			ValueType::Slice { .. } =>
			{
				self.equals(other) || self.can_coerce_into(other)
			}
			ValueType::SlicePointer { .. } =>
			{
				self.equals(other) || self.can_coerce_into(other)
			}
			ValueType::EndlessArray { .. } =>
			{
				self.equals(other) || self.can_coerce_into(other)
			}
			ValueType::Struct { .. } =>
			{
				self.equals(other) || self.can_coerce_into(other)
			}
			ValueType::View { deref_type } =>
			{
				self.equals(other)
					|| deref_type.as_ref().equals(other)
					|| deref_type.can_subautoderef_into(other)
					|| other
						.get_viewee_type()
						.map_or(false, |t| deref_type.can_subautoderef_into(&t))
			}
			ValueType::Pointer { deref_type } =>
			{
				self.equals(other)
					|| deref_type.as_ref().equals(other)
					|| deref_type.can_coerce_address_into(other)
					|| deref_type.can_subautoderef_into(other)
					|| other
						.get_pointee_type()
						.map_or(false, |t| deref_type.can_subautoderef_into(&t))
			}
			_ => false,
		}
	}

	fn can_subautoderef_into(&self, other: &Self) -> bool
	{
		match self
		{
			ValueType::View { deref_type } =>
			{
				deref_type.as_ref().equals(other)
					|| deref_type.can_subautoderef_into(other)
					|| other
						.get_viewee_type()
						.map_or(false, |t| deref_type.can_subautoderef_into(&t))
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.as_ref().equals(other)
					|| deref_type.can_subautoderef_into(other)
					|| other
						.get_pointee_type()
						.map_or(false, |t| deref_type.can_subautoderef_into(&t))
			}
			_ => false,
		}
	}

	pub fn is_wellformed(&self) -> bool
	{
		match self
		{
			ValueType::Void => true,
			ValueType::Array {
				element_type,
				length: _,
			} => element_type.is_wellformed_element(),
			ValueType::ArrayWithNamedLength {
				element_type,
				named_length: _,
			} => element_type.is_wellformed_element(),
			ValueType::Slice { element_type } =>
			{
				element_type.is_wellformed_element()
			}
			ValueType::SlicePointer { element_type } =>
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
		self.can_be_element() && self.is_wellformed_inner()
	}

	fn is_wellformed_inner(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::Array {
				element_type,
				length: _,
			} => element_type.is_wellformed_element(),
			ValueType::ArrayWithNamedLength {
				element_type,
				named_length: _,
			} => element_type.is_wellformed_element(),
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
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
			_ => self.is_wellformed(),
		}
	}

	fn can_be_element(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::Array { .. } => true,
			ValueType::ArrayWithNamedLength { .. } => true,
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => true,
			ValueType::Pointer { .. } => true,
			ValueType::View { .. } => false,
			_ => true,
		}
	}

	pub fn can_be_sized(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::Array { .. } => true,
			ValueType::ArrayWithNamedLength { .. } => true,
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Pointer { .. } => true,
			ValueType::View { .. } => false,
			_ => true,
		}
	}

	pub fn can_be_struct_member(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::View { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_word_member(&self) -> bool
	{
		self.can_be_struct_member()
			&& self.known_size_in_bytes_as_word_member().is_some()
	}

	pub fn can_be_constant(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_variable(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::View { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_parameter(&self) -> bool
	{
		match self
		{
			ValueType::Void => false,
			ValueType::Array { .. } => false,
			ValueType::ArrayWithNamedLength { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Struct { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn can_be_returned(&self) -> bool
	{
		match self
		{
			ValueType::Array { .. } => false,
			ValueType::ArrayWithNamedLength { .. } => false,
			ValueType::Slice { .. } => false,
			ValueType::SlicePointer { .. } => false,
			ValueType::EndlessArray { .. } => false,
			ValueType::Arraylike { .. } => false,
			ValueType::Struct { .. } => false,
			ValueType::UnresolvedStructOrWord { .. } => false,
			ValueType::View { .. } => false,
			_ => self.is_wellformed(),
		}
	}

	pub fn get_element_type(&self) -> Option<Self>
	{
		match self
		{
			ValueType::Array {
				element_type,
				length: _,
			} => Some(element_type.as_ref().clone()),
			ValueType::ArrayWithNamedLength {
				element_type,
				named_length: _,
			} => Some(element_type.as_ref().clone()),
			ValueType::Slice { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			ValueType::SlicePointer { element_type } =>
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

	pub fn get_pointee_type(&self) -> Option<Self>
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

	pub fn get_viewee_type(&self) -> Option<Self>
	{
		match self
		{
			ValueType::View { deref_type } => Some(deref_type.as_ref().clone()),
			_ => None,
		}
	}

	pub fn fully_dereferenced(&self) -> Self
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

	pub fn pointer_depth(&self) -> usize
	{
		self.add_pointer_depth(0)
	}

	fn add_pointer_depth(&self, total: usize) -> usize
	{
		match self
		{
			ValueType::Pointer { deref_type } =>
			{
				deref_type.add_pointer_depth(total + 1)
			}
			ValueType::SlicePointer { element_type: _ } => total + 1,
			_ => total,
		}
	}

	pub fn is_slice_pointer(&self) -> bool
	{
		match self
		{
			ValueType::SlicePointer { element_type: _ } => true,
			_ => false,
		}
	}

	pub fn min_i128(&self) -> i128
	{
		match self
		{
			ValueType::Int8 => i8::MIN as i128,
			ValueType::Int16 => i16::MIN as i128,
			ValueType::Int32 => i32::MIN as i128,
			ValueType::Int64 => i64::MIN as i128,
			ValueType::Int128 => i128::MIN as i128,
			ValueType::Uint8 => u8::MIN as i128,
			ValueType::Uint16 => u16::MIN as i128,
			ValueType::Uint32 => u32::MIN as i128,
			ValueType::Uint64 => u64::MIN as i128,
			ValueType::Uint128 => u128::MIN as i128,
			ValueType::Usize => u64::MIN as i128,
			ValueType::Char8 => u8::MIN as i128,
			ValueType::Pointer { .. } => u64::MIN as i128,
			ValueType::View { .. } => u64::MIN as i128,
			_ => 0,
		}
	}

	pub fn max_u128(&self) -> u128
	{
		match self
		{
			ValueType::Int8 => i8::MAX as u128,
			ValueType::Int16 => i16::MAX as u128,
			ValueType::Int32 => i32::MAX as u128,
			ValueType::Int64 => i64::MAX as u128,
			ValueType::Int128 => i128::MAX as u128,
			ValueType::Uint8 => u8::MAX as u128,
			ValueType::Uint16 => u16::MAX as u128,
			ValueType::Uint32 => u32::MAX as u128,
			ValueType::Uint64 => u64::MAX as u128,
			ValueType::Uint128 => u128::MAX as u128,
			ValueType::Usize => u64::MAX as u128,
			ValueType::Char8 => u8::MAX as u128,
			ValueType::Pointer { .. } => u64::MAX as u128,
			ValueType::View { .. } => u64::MAX as u128,
			_ => 0,
		}
	}

	pub fn for_string_literal(length: usize) -> Self
	{
		ValueType::Array {
			element_type: Box::new(ValueType::Char8),
			length,
		}
	}

	pub fn for_string_slice() -> Self
	{
		ValueType::Slice {
			element_type: Box::new(ValueType::Char8),
		}
	}
}
