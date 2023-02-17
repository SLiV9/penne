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

			rule %r(test.*?$), Comment::Single
		end

		state :comments do
			rule %r(//.*?$), Comment::Single
		end
	end
end
