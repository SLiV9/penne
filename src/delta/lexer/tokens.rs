use super::{BaseToken, LexingError, TokenPayload, ValueTypeKeyword};

use crate::alpha::error;
use crate::alpha::error::Errors;

use std::mem::MaybeUninit;

pub const MAX_SOURCE_LEN: usize = 1 << 31;
const MAX_NUM_TOKENS: usize = 1 << 24;
const MAX_NUM_PAYLOADS: usize = 1 << 24;
const MAX_NUM_LEXING_ERRORS: usize = 1024;

#[must_use]
pub(super) struct TokenAllocError;

#[derive(Clone, Copy, Debug)]
pub struct ValueTypeAndPayloadId
{
	value_type_and_payload_id: u32,
}

impl ValueTypeAndPayloadId
{
	fn new(
		value_type: ValueTypeKeyword,
		PayloadId(payload_id): PayloadId,
	) -> Self
	{
		let value_type = u32::from(value_type as u8);
		Self {
			value_type_and_payload_id: value_type | (payload_id << 8),
		}
	}

	#[inline(always)]
	pub fn value_type(self) -> ValueTypeKeyword
	{
		let value_type = (self.value_type_and_payload_id & 0xFF) as u8;
		let value_type = ValueTypeKeyword::from_repr(value_type);
		debug_assert!(value_type.is_some(), "ValueTypeAndPayloadId::new");
		value_type.unwrap_or(ValueTypeKeyword::NoKeyword)
	}

	#[inline(always)]
	pub fn payload_id(self) -> PayloadId
	{
		let payload_id = self.value_type_and_payload_id >> 8;
		// PayloadId validity is checked on use.
		PayloadId(payload_id)
	}
}

#[derive(Clone, Copy, Debug)]
pub struct TokenId(u32);

#[derive(Clone, Copy, Debug)]
pub struct PayloadId(u32);

#[derive(Clone, Copy, Debug)]
pub struct TokenLocation
{
	pub(super) start: u32,
	pub(super) end: u32,
	pub(super) start_of_line: u32,
	pub(super) line_number: u32,
}

impl TokenLocation
{
	pub fn span(&self) -> std::ops::Range<usize>
	{
		self.span_from(self.start)
	}

	pub(super) fn span_from(&self, from: u32) -> std::ops::Range<usize>
	{
		let start = from as usize;
		let end = self.end as usize;
		start..end
	}

	pub fn line_number(&self) -> usize
	{
		self.line_number as usize
	}

	pub fn line_offset(&self) -> usize
	{
		(self.start - self.start_of_line) as usize
	}
}

pub struct Tokens
{
	pub(super) source_filename: String,

	// Tokens (SOA)
	tokens: Vec<BaseToken>,
	token_vaps: Vec<ValueTypeAndPayloadId>,
	token_locations: Vec<TokenLocation>,

	integer_payloads: Vec<u128>,
	errors: Vec<(LexingError, TokenId)>,
}

impl Tokens
{
	pub(super) fn empty(source_filename: String, source_len: usize) -> Tokens
	{
		// For tokens, we want to avoid the realloc at all costs.
		// If the source is very big, we expect a medium density.
		let hard_token_cap = std::cmp::max(source_len / 4, 1 << 16);
		let token_cap = std::cmp::min(hard_token_cap, MAX_NUM_TOKENS);
		let tokens = Vec::with_capacity(token_cap);
		let token_vaps = Vec::with_capacity(token_cap);
		let token_locations = Vec::with_capacity(token_cap);

		// Don't pre-allocate too many payloads. We will rescale these.
		let soft_payload_cap = std::cmp::max(source_len / 64, 1024);
		let payload_cap = std::cmp::min(soft_payload_cap, MAX_NUM_PAYLOADS);
		let integer_payloads = Vec::with_capacity(payload_cap);

		// For errors we have MAX_NUM_LEXING_ERRORS as a hard cap
		// because there is no point showing the user millions of errors.
		let error_cap = std::cmp::min(source_len, MAX_NUM_LEXING_ERRORS);
		let errors = Vec::with_capacity(error_cap);

		Tokens {
			source_filename,
			tokens,
			token_vaps,
			token_locations,
			integer_payloads,
			errors,
		}
	}

	#[inline(never)]
	#[cold]
	pub(super) fn empty_with_one_error(
		source_filename: String,
		error: LexingError,
	) -> Tokens
	{
		let mut tokens = Self::empty(source_filename, 0);
		tokens.tokens.push(BaseToken::Error);
		tokens.token_vaps.push(ValueTypeAndPayloadId::new(
			ValueTypeKeyword::NoKeyword,
			PayloadId(0),
		));
		tokens.token_locations.push(TokenLocation {
			start: 0,
			end: 0,
			start_of_line: 0,
			line_number: 0,
		});
		tokens.errors.push((error, TokenId(0)));
		tokens
	}

	pub(super) fn buffer(&mut self) -> TokensBuffer<'_>
	{
		let Self {
			source_filename: _,
			tokens,
			token_vaps,
			token_locations,
			integer_payloads,
			errors,
		} = self;
		assert_eq!(token_vaps.len(), tokens.len());
		assert_eq!(token_locations.len(), tokens.len());
		assert_eq!(token_vaps.capacity(), tokens.capacity());
		assert_eq!(token_locations.capacity(), tokens.capacity());
		assert_eq!(tokens.len(), 0);
		assert_eq!(integer_payloads.len(), 0);
		assert_eq!(errors.len(), 0);
		TokensBuffer {
			num_tokens: 0,
			tokens: tokens.spare_capacity_mut(),
			token_vaps: token_vaps.spare_capacity_mut(),
			token_locations: token_locations.spare_capacity_mut(),
			integer_payloads,
			num_errors: 0,
			errors: errors.spare_capacity_mut(),
		}
	}

	/// TODO
	pub(super) unsafe fn set_tokens_len(&mut self, num_tokens: usize)
	{
		assert_eq!(self.token_vaps.len(), self.tokens.len());
		assert_eq!(self.token_locations.len(), self.tokens.len());
		assert_eq!(self.token_vaps.capacity(), self.tokens.capacity());
		assert_eq!(self.token_locations.capacity(), self.tokens.capacity());

		assert_eq!(self.tokens.len(), 0);
		assert!(num_tokens <= self.tokens.capacity());
		// TODO
		unsafe {
			self.tokens.set_len(num_tokens);
			self.token_vaps.set_len(num_tokens);
			self.token_locations.set_len(num_tokens);
		}
	}

	/// TODO
	pub(super) unsafe fn set_errors_len(&mut self, num_errors: usize)
	{
		assert_eq!(self.errors.len(), 0);
		assert!(num_errors <= self.errors.capacity());
		// TODO
		unsafe {
			self.errors.set_len(num_errors);
		}
	}
}

pub(super) struct TokensBuffer<'buffer>
{
	pub num_tokens: usize,
	tokens: &'buffer mut [MaybeUninit<BaseToken>],
	token_vaps: &'buffer mut [MaybeUninit<ValueTypeAndPayloadId>],
	token_locations: &'buffer mut [MaybeUninit<TokenLocation>],

	integer_payloads: &'buffer mut Vec<u128>,

	pub num_errors: usize,
	errors: &'buffer mut [MaybeUninit<(LexingError, TokenId)>],
}

impl<'buffer> TokensBuffer<'buffer>
{
	#[inline]
	pub(super) fn push(
		&mut self,
		base_token: BaseToken,
		value_type: Option<ValueTypeKeyword>,
		payload: Option<TokenPayload>,
		location: TokenLocation,
	) -> Result<TokenId, TokenAllocError>
	{
		let value_type = value_type.unwrap_or(ValueTypeKeyword::NoKeyword);
		let payload_id = match payload
		{
			Some(TokenPayload::Integer(payload)) =>
			{
				self.push_integer_payload(payload)?
			}
			None => PayloadId(0),
		};
		self.push_token(base_token, value_type, payload_id, location)
	}

	fn push_token(
		&mut self,
		base_token: BaseToken,
		value_type: ValueTypeKeyword,
		payload_id: PayloadId,
		location: TokenLocation,
	) -> Result<TokenId, TokenAllocError>
	{
		let i = self.num_tokens;
		if i >= self.tokens.len()
		{
			return Err(TokenAllocError);
		}
		let token_id = TokenId(i as u32);
		let vap = ValueTypeAndPayloadId::new(value_type, payload_id);
		self.tokens[i].write(base_token);
		self.token_vaps[i].write(vap);
		self.token_locations[i].write(location);
		self.num_tokens += 1;
		Ok(token_id)
	}

	fn push_integer_payload(
		&mut self,
		payload: u128,
	) -> Result<PayloadId, TokenAllocError>
	{
		let i = self.integer_payloads.len();
		if i >= MAX_NUM_PAYLOADS
		{
			return Err(TokenAllocError);
		}
		let payload_id = PayloadId(i as u32);
		self.integer_payloads.push(payload);
		Ok(payload_id)
	}

	pub(super) fn push_error(
		&mut self,
		error: LexingError,
		location: TokenLocation,
	) -> Result<(), TokenAllocError>
	{
		let i = self.num_errors;
		if i >= self.errors.len()
		{
			// We ignore errors after the first MAX_NUM_LEXING_ERRORS,
			// because there is no point showing the user all of them.
			return Ok(());
		}
		let token_id = self.push(BaseToken::Error, None, None, location)?;
		self.errors[i].write((error, token_id));
		self.num_errors += 1;
		Ok(())
	}

	pub(super) fn push_end_of_source(
		&mut self,
		end_of_source_location: TokenLocation,
	) -> Result<(), TokenAllocError>
	{
		self.push(BaseToken::EndOfSource, None, None, end_of_source_location)?;
		Ok(())
	}
}

impl Tokens
{
	pub fn get_value_type_and_payload(
		&self,
		TokenId(token_id): TokenId,
	) -> ValueTypeAndPayloadId
	{
		let i = token_id as usize;
		// This cannot fail, by construction.
		self.token_vaps[i]
	}

	pub fn get_integer_payload(
		&self,
		PayloadId(payload_id): PayloadId,
	) -> Option<&u128>
	{
		if payload_id == 0
		{
			return None;
		}
		self.integer_payloads.get(payload_id as usize)
	}

	pub fn get_location(&self, TokenId(token_id): TokenId) -> error::Location
	{
		let i = token_id as usize;
		// This cannot fail, by construction.
		let location = self.token_locations[i];
		error::Location {
			source_filename: self.source_filename.to_string(),
			span: location.span(),
			line_number: location.line_number(),
			line_offset: location.line_offset(),
		}
	}

	pub fn errors(&self) -> Option<Errors>
	{
		if self.errors.is_empty()
		{
			return None;
		}

		let errors = (self.errors.iter().copied())
			.map(|(error, token_id)| {
				let location = self.get_location(token_id);
				let expectation = "Invalid token".to_string();
				error::Error::Lexical {
					error,
					location,
					expectation,
				}
			})
			.collect();

		Some(Errors { errors })
	}
}
