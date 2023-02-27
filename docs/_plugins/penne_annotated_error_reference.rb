require 'tempfile'

module Jekyll
	class PenneAnnotatedErrorReference < Liquid::Block
		include Liquid::StandardFilters

		def initialize(tag_name, text, tokens)
			super
		end

		def render(context)
			input = super
			current_error_code = nil
			current_code_sample = nil
			is_erroneous_code_sample = false
			is_snippet = false
			in_code_block = false;
			output = ""
			input.each_line do |line|
				output << line
				line.match(/^## Error code E(?<code>[0-9]+)\b.*/) do |m|
					current_error_code = m[:code]
					is_erroneous_code_sample = false
				end
				if line.match?(/^### Example of erroneous code/)
					is_erroneous_code_sample = true
				end
				if line.match?(/^```/) and in_code_block
					in_code_block = false
					if is_erroneous_code_sample
						output << get_error_message_html(current_code_sample,
							current_error_code)
						is_erroneous_code_sample = false
					elsif not is_snippet
						expect_no_errors(current_code_sample,
							current_error_code)
					end
				end
				if in_code_block
					current_code_sample << line
					is_snippet &&= line.match?(/^(  |\t)/)
				end
				if line.match?(/^```penne/) and not current_error_code.nil?
					in_code_block = true
					is_snippet = true
					current_code_sample = ""
				end
			end
			return output
		end

		def get_error_message_html(code_sample, expected_error_code)
			html = ""
			file = Tempfile.new(['penne_', '_sample.pn'])
			file.write(code_sample)
			file.close
			begin
				emitted_html = `penne emit "#{file.path}" 2>&1 > /dev/null | ansi2html.sh --body-only 2>/dev/null`
				if $? != 0
					raise "Failed to get errors for E#{expected_error_code}"
				end
				emitted_html.sub!(/\/tmp\/penne_[a-zA-Z0-9._-]+_sample\.pn/,
					'&lt;source&gt;')
				emitted_html.sub!(/\nError: compilation failed\n?\z/, '')
				html << '<pre class="terminal-output f9 b9">' + "\n"
				html << emitted_html
				html << '</pre>' + "\n"
			ensure
				file.close
				file.unlink
			end
			return html
		end

		def expect_no_errors(code_sample, fixed_error_code)
			file = Tempfile.new(['penne_', '_sample.pn'])
			file.write(code_sample)
			file.close
			begin
				`penne emit "#{file.path}" > /dev/null`
				if $? != 0
					raise "Failed fix for E#{fixed_error_code}"
				end
			ensure
				file.unlink
			end
		end
	end
end

puts "Registering PenneAnnotatedErrorReference block..."
Liquid::Template.register_tag('annotatederrorreference',
	Jekyll::PenneAnnotatedErrorReference)
