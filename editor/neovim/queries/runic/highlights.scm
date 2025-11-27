; Tree-sitter highlight queries for Runic.
; These captures mirror the Runic highlight tables in
; lua/runic_highlight/highlights.lua. They target the node names that the
; upcoming tree-sitter-runic grammar will expose. When the grammar lands, adjust
; the node names here if they differ, but keep the capture names intact.

; Keywords --------------------------------------------------------------------

(keyword_declaration) @keyword.modifier
(keyword_type) @keyword.type
(keyword_async) @keyword.coroutine
(keyword_control) @keyword.control
(keyword_loop) @keyword.repeat
(keyword_import) @keyword.import
(keyword_interop) @keyword.special
(keyword_try) @keyword.exception

; Literals & builtins ---------------------------------------------------------

(boolean_literal) @boolean
(null_literal) @number
(number_literal) @number
(string_literal) @string
(string_fragment) @string
(escape_sequence) @string.escape
(interpolation_expression
  "${" @string.special.symbol
  (_) @embedded
  "}" @string.special.symbol)
(rune_literal) @character
;(capture_clause) @keyword.capture
(builtin_command) @function.builtin
(type_identifier) @type

; Operators & punctuation -----------------------------------------------------

(pipeline_operator) @keyword.operator
(process_operator) @keyword.operator
(logical_operator) @keyword.operator
(assignment_operator) @keyword.operator
(comparison_operator) @keyword.operator
(range_operator) @keyword.operator
(arrow_operator) @keyword.operator
(sigil) @keyword.operator
(bracket) @punctuation.bracket
(punctuation_delimiter) @punctuation.delimiter

; Comments & structure --------------------------------------------------------

(line_comment) @comment
(documentation_comment) @comment.documentation
(block_comment) @comment.block
(command_head) @function.call
(stage_separator) @punctuation.special
