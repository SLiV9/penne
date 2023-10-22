Jekyll::Hooks.register :site, :pre_render do |site|
	puts "Registering Penne lexer..."
	require "rouge"

	class Penne < Rouge::RegexLexer
		title 'Penne'
		desc 'Penne Programming Language'
		tag 'penne'
		filenames '*.pn'

		def self.identifier
			@identifier = %r/[[:alpha:]][_[:alnum:]]*|_[_[:alnum:]]+/
		end

		def self.int_suffixes
			@int_suffixes ||= %w(i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 usize)
		end

		def self.float_suffixes
			@float_suffixes ||= %w(f32 f64)
		end

		def self.word_keywords
			@word_keywords ||= %w(word8 word16 word32 word64 word128)
		end

		def self.type_keywords
			@type_keywords ||= %w(void bool char8 struct) +
				Penne.word_keywords +
				Penne.int_suffixes +
				Penne.float_suffixes
		end

		def self.literal_keywords
			@literal_keywords ||= %w(true false)
		end

		def self.hard_keywords
			@hard_keywords ||= %w(
				fn var const if goto loop else cast as import pub extern
			)
		end

		def self.keywords
			@keywords ||= Penne.hard_keywords + Penne.type_keywords +
				Penne.literal_keywords
		end

		state :root do
			mixin :comments
			mixin :values
			mixin :declarations
			mixin :labels_in_sample
			mixin :main
			mixin :whitespace
			mixin :fallback
		end

		state :values do
			rule %r/\$.*?$/, Error
			rule %r/"/, Str, :double_quoted_string
			rule %r/'/, Str, :single_quoted_string
			rule %r/\b(-)?[0-9][.0-9_]*(?=(i|u))/, Num::Integer
			rule %r/\b(-)?[0-9][.0-9_]*\b/, Num::Integer
			rule %r/\b0x[0-9a-fA-F_]+(?=(i|u))/, Num::Integer
			rule %r/\b0x[0-9a-fA-F_]+\b/, Num::Integer
			rule %r/\b0b[0-1_]+(?=(i|u))/, Num::Integer
			rule %r/\b0b[0-1_]+\b/, Num::Integer
			rule %r/\b(true|false)\b/, Keyword::Constant
		end

		state :declarations do
			rule %r/\b(fn)\b/, Keyword::Declaration, :function_identifier
			rule %r/\b(const)\b/, Keyword::Declaration, :constant_identifier
			rule %r/\b(struct|#{Penne.word_keywords.join('|')})\b/, Keyword::Declaration, :struct_identifier
		end

		state :main do
			rule %r/\b(var)\b/, Keyword::Pseudo, :var_identifier
			rule %r/\b(cast|as)\b/, Operator::Word
			rule %r/\b(#{Penne.type_keywords.join('|')})\b/, Keyword::Type
			rule %r/\b(pub|extern)\b/, Keyword::Pseudo
			rule %r/\b(#{Penne.keywords.join('|')})\b/, Keyword
			rule %r/\b(#{Penne.identifier})(?=\s*\{)/, Text, :structural
			rule %r/\{/, Text, :block_body
		end

		state :whitespace do
			rule %r/[\s]+/, Text
		end

		state :fallback do
			rule %r/[a-zA-Z][_a-zA-Z0-9]*/, Text
			rule %r/_[_a-zA-Z0-9]+/, Text
			rule %r/(==|!=|>=|<=|<<|>>|->)/, Text
			rule %r/[!#%&()*+,\-.\/:;<=>?@\[\]^_`{|}~]/, Text
		end

		state :comments do
			rule %r(//.*?$), Comment::Single
		end

		state :double_quoted_string do
			rule %r/"/, Str, :pop!
			rule %r/\\x[0-9a-fA-F]{2}/, Str::Escape
			rule %r/\\u\{[0-9a-fA-F]{1,}\}/, Str::Escape
			rule %r/\\./, Str::Escape
			rule %r/[^"\\]+/, Str
			rule %r/\\$/, Error
			rule %r/$/, Error, :pop!
		end

		state :single_quoted_string do
			rule %r/'/, Str, :pop!
			rule %r/\\x[0-9a-fA-F]{2}/, Str::Escape
			rule %r/\\./, Str::Escape
			rule %r/[^'\\]+/, Str
			rule %r/\\$/, Error
			rule %r/$/, Error, :pop!
		end

		state :var_identifier do
			rule %r/\b(#{Penne.keywords.join('|')})\b/, Keyword, :pop!
			rule %r/\b#{Penne.identifier}\b/, Name::Variable, :pop!
			mixin :comments
			mixin :whitespace
			rule %r/[^\w]+/, Error, :pop!
		end

		state :constant_identifier do
			rule %r/\b(#{Penne.keywords.join('|')})\b/, Keyword, :pop!
			rule %r/\b#{Penne.identifier}\b/, Name::Constant, :pop!
			mixin :comments
			mixin :whitespace
			rule %r/[^\w]+/, Error, :pop!
		end

		state :struct_identifier do
			rule %r/\b(#{Penne.keywords.join('|')})\b/, Keyword, :pop!
			rule %r/\b#{Penne.identifier}\b/ do
				token Name::Class
				pop!
				push :structural
			end
			mixin :comments
			mixin :whitespace
			rule %r/[^\w]+/, Error, :pop!
		end

		state :function_identifier do
			rule %r/\b(#{Penne.keywords.join('|')})\b/, Keyword, :pop!
			rule %r/\b#{Penne.identifier}\b/ do
				token Name::Function
				pop!
				push :function_signature
			end
			mixin :comments
			mixin :whitespace
			rule %r/[^\w]+/, Error, :pop!
		end

		state :function_signature do
			rule %r/;/, Text, :pop!
			rule %r/\{/ do
				token Text
				pop!
				push :block_body
			end
			mixin :comments
			mixin :values
			mixin :main
			mixin :whitespace
			mixin :fallback
		end

		state :block_body do
			rule %r/\}/, Text, :pop!
			rule %r/\b(#{Penne.keywords.join('|')})(?=:)/, Keyword
			rule %r/\b#{Penne.identifier}:/, Name::Label
			mixin :comments
			mixin :values
			mixin :main
			mixin :whitespace
			mixin :fallback
		end

		state :labels_in_sample do
			rule %r/\b(#{Penne.keywords.join('|')})(?=:)/, Keyword
			rule %r/\b#{Penne.identifier}:/, Name::Label
		end

		state :structural do
			rule %r/\{/ do
				token Text
				pop!
				push :structural_body
			end
			rule %r/;/, Text, :pop!
			mixin :comments
			mixin :whitespace
			mixin :fallback
		end

		state :structural_body do
			rule %r/\}/, Text, :pop!
			mixin :comments
			mixin :values
			mixin :main
			mixin :whitespace
			mixin :fallback
		end
	end
end
