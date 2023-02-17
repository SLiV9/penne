Jekyll::Hooks.register :site, :pre_render do |site|
	puts "Registering PenneLexer..."
	require "rouge"

	class PenneLexer < Rouge::RegexLexer
		title 'Penne'
		desc 'Penne Programming Language'
		tag 'penne'
		filenames '*.pn'

		primitives = %w(i32)

		state :root do
			mixin :comments

			rule %r/\$.*?$/, Generic::Error
			rule %r/"/, Str, :double_quoted_string
			rule %r/'/, Str, :single_quoted_string
			rule %r/\b(-)?[0-9][.0-9]*\b/, Num::Integer
			rule %r/\b0x[0-9a-fA-F]+\b/, Num::Integer
			rule %r/\b0b[0-1]+\b/, Num::Integer
			rule %r/\b(true|false)\b/, Keyword::Constant

			rule %r/[\s]+/, Text::Whitespace
			rule %r/[a-zA-Z][_a-zA-Z0-9]*/, Text
			rule %r/_[_a-zA-Z0-9]+/, Text
			rule %r/[!#%&()*+,\-.\/:;<=>?@\[\]^_`{|}~]+/, Text
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
			rule %r/\\$/, Generic::Error
			rule %r/$/, Generic::Error, :pop!
		end

		state :single_quoted_string do
			rule %r/'/, Str, :pop!
			rule %r/\\x[0-9a-fA-F]{2}/, Str::Escape
			rule %r/\\./, Str::Escape
			rule escapes, Str::Escape
			rule %r/[^'\\]+/, Str
			rule %r/\\$/, Generic::Error
			rule %r/$/, Generic::Error, :pop!
		end
	end
end
