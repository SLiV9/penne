use super::{BaseToken, LexingError, TokenPayload, ValueTypeKeyword};

use crate::alpha::error;
use crate::alpha::error::Errors;

pub const MAX_SOURCE_LEN: usize = 1 << 31;
const MAX_NUM_TOKENS: usize = MAX_NUM_PAYLOADS - 1;
const MAX_NUM_PAYLOADS: usize = 1 << 24;

#[derive(Clone, Copy, Debug)]
pub struct Token
{
	base_token_and_token_id: u32,
	value_type_and_payload_id: u32,
}

impl Token
{
	fn new(
		base_token: BaseToken,
		TokenId(token_id): TokenId,
		value_type: ValueTypeKeyword,
		PayloadId(payload_id): PayloadId,
	) -> Token
	{
		let base_token = u32::from(base_token as u8);
		let value_type = u32::from(value_type as u8);
		Token {
			base_token_and_token_id: base_token | (token_id << 8),
			value_type_and_payload_id: value_type | (payload_id << 8),
		}
	}

	pub fn base_token(self) -> BaseToken
	{
		let base_token = (self.base_token_and_token_id & 0xFF) as u8;
		BaseToken::from_repr(base_token).expect("from Token::new")
	}

	pub fn token_id(self) -> TokenId
	{
		let token_id = self.base_token_and_token_id >> 8;
		TokenId(token_id)
	}

	pub fn value_type(self) -> ValueTypeKeyword
	{
		let value_type = (self.value_type_and_payload_id & 0xFF) as u8;
		ValueTypeKeyword::from_repr(value_type).expect("from Token::new")
	}

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

impl PayloadId
{
	const NONE: PayloadId = PayloadId(0);
}

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
	source_filename: String,

	// Tokens (SOA)
	tokens: Vec<Token>,
	token_locations: Vec<TokenLocation>,

	payloads: Vec<TokenPayload>,
	errors: Vec<(LexingError, TokenId)>,
}

impl Tokens
{
	pub(super) fn empty(source_filename: String, source_len: usize) -> Tokens
	{
		let num_tokens = source_len / 4;
		let mut tokens = Vec::with_capacity(num_tokens);
		let mut token_locations = Vec::with_capacity(num_tokens);
		let mut payloads = Vec::with_capacity(source_len / 10);
		let mut errors = Vec::with_capacity(source_len / 100);

		// This payload is unreachable because 0 is an invalid PayloadId.
		payloads.push(TokenPayload::UnreachablePayload);

		Tokens {
			source_filename,
			tokens,
			token_locations,
			payloads,
			errors,
		}
	}

	#[inline]
	pub(super) fn push(
		&mut self,
		base_token: BaseToken,
		value_type: Option<ValueTypeKeyword>,
		payload: Option<TokenPayload>,
		location: TokenLocation,
	) -> TokenId
	{
		let value_type = value_type.unwrap_or(ValueTypeKeyword::NoKeyword);
		let payload_id = if let Some(payload) = payload
		{
			self.push_payload(payload)
		}
		else
		{
			PayloadId::NONE
		};
		self.push_token(base_token, value_type, payload_id, location)
	}

	fn push_token(
		&mut self,
		base_token: BaseToken,
		value_type: ValueTypeKeyword,
		payload_id: PayloadId,
		location: TokenLocation,
	) -> TokenId
	{
		// TokenId validity is checked upon use.
		let token_id = TokenId(self.tokens.len() as u32);
		let token = Token::new(base_token, token_id, value_type, payload_id);
		self.tokens.push(token);
		self.token_locations.push(location);
		token_id
	}

	fn push_payload(&mut self, payload: TokenPayload) -> PayloadId
	{
		// PayloadId validity is checked upon use.
		let payload_id = PayloadId(self.payloads.len() as u32);
		self.payloads.push(payload);
		payload_id
	}

	pub(super) fn push_error(
		&mut self,
		error: LexingError,
		location: TokenLocation,
	)
	{
		let token_id = self.push(BaseToken::Error, None, None, location);
		self.errors.push((error, token_id));
	}

	pub(super) fn finalize(
		mut self,
		end_of_source_location: TokenLocation,
	) -> Self
	{
		if self.tokens.len() > MAX_NUM_TOKENS - 1
		{
			self.tokens.truncate(MAX_NUM_TOKENS - 1);
			self.token_locations.truncate(MAX_NUM_TOKENS - 1);

			let last = self.tokens.last_mut().expect("clearly non-empty");
			let error = LexingError::TooManyTokens;
			self.errors.push((error, last.token_id()));
		}
		self.push(BaseToken::EndOfSource, None, None, end_of_source_location);

		assert!(self.tokens.len() <= MAX_NUM_TOKENS);
		assert_eq!(self.token_locations.len(), self.tokens.len());
		assert!(self.payloads.len() <= 1 + self.tokens.len());
		assert!(self.payloads.len() <= MAX_NUM_PAYLOADS);
		self
	}

	pub fn get_payload(
		&self,
		PayloadId(payload_id): PayloadId,
	) -> Option<&TokenPayload>
	{
		if payload_id == 0
		{
			return None;
		}
		self.payloads.get(payload_id as usize)
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

	pub fn dump(&self) -> impl Iterator<Item = String>
	{
		self.tokens.iter().copied().map(|token| {
			let base_token = token.base_token();
			let value_type = token.value_type();
			let payload = self.get_payload(token.payload_id());
			match (value_type, payload)
			{
				(ValueTypeKeyword::NoKeyword, None) => format!("{base_token}"),
				(ValueTypeKeyword::NoKeyword, Some(payload)) =>
				{
					format!("{base_token}={payload:?}")
				}
				(value_type, None) => format!("{base_token}({value_type})"),
				(value_type, Some(payload)) =>
				{
					format!("{base_token}({value_type})={payload:?}")
				}
			}
		})
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
