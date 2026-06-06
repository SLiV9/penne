use std::ops::{Deref, DerefMut};

use crate::delta::lexer;
use crate::delta::lexer::BaseToken;
use crate::delta::lexer::BaseToken::EndOfSource;
use crate::delta::parser::ParsingError;

pub struct Tokens<'a>
{
	tokens: &'a lexer::tokens::Tokens,
	next_token_id: lexer::tokens::TokenId,
	span: &'a [BaseToken],
}

impl<'a> From<&'a lexer::tokens::Tokens> for Tokens<'a>
{
	fn from(tokens: &'a lexer::tokens::Tokens) -> Self
	{
		Self {
			tokens,
			next_token_id: tokens.first_token_id(),
			span: tokens.base_tokens(),
		}
	}
}

impl<'a> Tokens<'a>
{
	#[inline(always)]
	pub fn starting_from(&'a self, from: lexer::tokens::TokenId) -> Tokens<'a>
	{
		Tokens {
			tokens: self.tokens,
			next_token_id: from,
			span: self.tokens.base_tokens_from(from),
		}
	}
}

impl<'a, 'b: 'a> Tokens<'b>
{
	#[inline(always)]
	pub fn with_reservation(
		&'a mut self,
		reserved: impl Fn(BaseToken) -> bool,
	) -> TokensWithReservation<'a, 'b>
	{
		let end = self.find_next(reserved);
		let temporary = Tokens {
			tokens: self.tokens,
			next_token_id: self.next_token_id,
			span: self.tokens.base_tokens_of_span(self.next_token_id..end),
		};
		TokensWithReservation {
			source: self,
			temporary,
		}
	}
}

impl<'a> Tokens<'a>
{
	#[inline(always)]
	pub fn cursor(&self) -> lexer::tokens::TokenId
	{
		self.next_token_id
	}

	pub fn get_value_type_and_payload(
		&self,
		token_id: lexer::tokens::TokenId,
	) -> lexer::tokens::ValueTypeAndPayloadId
	{
		self.tokens.get_value_type_and_payload(token_id)
	}

	#[inline(always)]
	pub fn spans_multiple_tokens(&self, span: lexer::tokens::Span) -> bool
	{
		self.tokens.spans_multiple_tokens(span)
	}

	#[inline(always)]
	pub fn find_next(
		&'a self,
		until: impl Fn(BaseToken) -> bool,
	) -> lexer::tokens::TokenId
	{
		self.tokens.skip_until(until, self.next_token_id)
	}

	#[inline(always)]
	pub fn peek(&self) -> BaseToken
	{
		self.span.first().copied().unwrap_or(EndOfSource)
	}

	#[inline(always)]
	pub fn take(&mut self) -> BaseToken
	{
		self.tokens.advance(&mut self.next_token_id);
		self.span.split_off_first().copied().unwrap_or(EndOfSource)
	}

	#[inline(always)]
	pub fn consume(&mut self, expected: BaseToken) -> Result<(), ParsingError>
	{
		let token_id = self.next_token_id;
		let token = self.take();
		if token == expected
		{
			Ok(())
		}
		else
		{
			let expectation = match expected
			{
				BaseToken::Assignment => "Expected assignment.",
				BaseToken::BraceLeft => "Expected opening brace.",
				BaseToken::BraceRight => "Expected closing brace.",
				BaseToken::BracketLeft => "Expected opening bracket.",
				BaseToken::BracketRight => "Expected closing bracket.",
				BaseToken::Dot => "Expected dot.",
				BaseToken::ParenLeft => "Expected opening parenthesis.",
				BaseToken::ParenRight => "Expected closing parenthesis.",
				BaseToken::Pipe => "Expected pipe.",
				BaseToken::Semicolon => "Expected semicolon.",
				BaseToken::StringLiteral => "Expected string literal.",
				BaseToken::Identifier => "Expected identifier.",
				_ => unreachable!(),
			};
			Err(ParsingError::UnexpectedToken {
				token: token_id,
				expectation,
			})
		}
	}

	#[inline(always)]
	pub fn consume_optional(&mut self, expected: BaseToken) -> bool
	{
		let found = self.peek() == expected;
		if found
		{
			self.take();
		}
		found
	}
}

pub struct TokensWithReservation<'a, 'b: 'a>
{
	source: &'a mut Tokens<'b>,
	temporary: Tokens<'a>,
}

impl<'a, 'b: 'a> Drop for TokensWithReservation<'a, 'b>
{
	fn drop(&mut self)
	{
		let from = self.temporary.next_token_id;
		self.source.next_token_id = from;
		self.source.span = self.source.tokens.base_tokens_from(from);
	}
}

impl<'a, 'b: 'a> Deref for TokensWithReservation<'a, 'b>
{
	type Target = Tokens<'a>;

	fn deref(&self) -> &Self::Target
	{
		&self.temporary
	}
}

impl<'a, 'b: 'a> DerefMut for TokensWithReservation<'a, 'b>
{
	fn deref_mut(&mut self) -> &mut Self::Target
	{
		&mut self.temporary
	}
}
