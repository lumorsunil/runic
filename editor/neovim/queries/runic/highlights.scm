; Tree-sitter highlight queries for Runic.
; These captures mirror the Runic highlight tables in
; lua/runic_highlight/highlights.lua. They target the node names that the
; upcoming tree-sitter-runic grammar will expose. When the grammar lands, adjust
; the node names here if they differ, but keep the capture names intact.

; Keywords --------------------------------------------------------------------

(keyword_declaration) @keyword.declaration.runic
(keyword_type) @keyword.type.runic
(keyword_async) @keyword.coroutine.runic
(keyword_control) @keyword.control.runic
(keyword_loop) @keyword.repeat.runic
(keyword_import) @keyword.import.runic
(keyword_interop) @keyword.special.runic
(keyword_error) @keyword.exception.runic
(boolean_literal) @constant.builtin.boolean.runic
(null_literal) @constant.builtin.nil.runic

; Literals & builtins ---------------------------------------------------------

(number_literal) @number.runic
(string_literal) @string.runic
(escape_sequence) @string.escape.runic
(interpolation_expression
  "${" @string.special.symbol.runic
  (_) @embedded.runic
  "}" @string.special.symbol.runic)
(rune_literal) @character.runic
(capture_clause) @keyword.capture.runic
(builtin_command) @function.builtin.runic
(type_identifier) @type.identifier.runic

; Operators & punctuation -----------------------------------------------------

(pipeline_operator) @operator.pipeline.runic
(process_operator) @operator.process.runic
(logical_operator) @operator.logical.runic
(assignment_operator) @operator.assignment.runic
(comparison_operator) @operator.comparison.runic
(range_operator) @operator.range.runic
(arrow_operator) @operator.arrow.runic
(sigil) @operator.sigil.runic
(bracket) @punctuation.bracket.runic
(punctuation_delimiter) @punctuation.delimiter.runic

; Comments & structure --------------------------------------------------------

(line_comment) @comment.line.runic
(documentation_comment) @comment.documentation.runic
(block_comment) @comment.block.runic
(command_head) @function.call.runic
(stage_separator) @punctuation.special.runic
(heredoc_fence) @string.special.runic
