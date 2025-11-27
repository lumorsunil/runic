// Minimal Tree-sitter grammar stub to keep highlight queries wired up.
// Node names mirror editor/neovim/queries/runic/highlights.scm so Neovim can
// load captures even before the full Runic parser exists.
const PREC = {
  command: 1,
};

module.exports = grammar({
  name: 'runic',

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
    $.documentation_comment,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.logical_operator, $.sigil],
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.command,
      // $.heredoc_fence,
      $.stage_separator,
      $._expression_atom
    )),

    command: $ => prec.left(PREC.command, seq(
      choice($.builtin_command, $.command_head),
      repeat($._argument)
    )),

    _argument: $ => choice(
      $.keyword_type,
      $.keyword_async,
      $.keyword_control,
      $.keyword_loop,
      $.keyword_import,
      $.keyword_interop,
      $.keyword_try,
      $.boolean_literal,
      $.null_literal,
      $.number_literal,
      $.string_literal,
      $.rune_literal,
      $.type_identifier,
      $.pipeline_operator,
      $.process_operator,
      $.logical_operator,
      $.assignment_operator,
      $.comparison_operator,
      $.range_operator,
      $.arrow_operator,
      $.sigil,
      $.punctuation_delimiter,
      $.bracket,
      $.identifier,
      // $.heredoc_fence
    ),

    _expression_atom: $ => choice(
      $.keyword_declaration,
      $.keyword_type,
      $.keyword_async,
      $.keyword_control,
      $.keyword_loop,
      $.keyword_import,
      $.keyword_interop,
      $.keyword_try,
      $.boolean_literal,
      $.null_literal,
      $.number_literal,
      $.string_literal,
      $.rune_literal,
      $.builtin_command,
      $.type_identifier,
      $.pipeline_operator,
      $.process_operator,
      $.logical_operator,
      $.assignment_operator,
      $.comparison_operator,
      $.range_operator,
      $.arrow_operator,
      $.sigil,
      $.bracket,
      $.punctuation_delimiter,
      $.identifier,
      // $.heredoc_fence
    ),

    keyword_declaration: _ => choice('const', 'var', 'fn'),
    keyword_type: _ => choice('error', 'enum', 'union'),
    keyword_async: _ => choice('async', 'await'),
    keyword_control: _ => choice('if', 'else', 'match', 'return'),
    keyword_loop: _ => choice('for', 'while'),
    keyword_import: _ => choice('import', 'from'),
    keyword_interop: _ => 'bash',
    keyword_try: _ => 'try',
    keyword_catch: _ => 'catch',

    boolean_literal: _ => choice('true', 'false'),
    null_literal: _ => 'null',

    number_literal: _ => token(choice(
      seq(/0[xX]/, /[0-9a-fA-F_]+/),
      seq(
        /\d+(_\d+)*/,
        optional(seq('.', /\d+(_\d+)*/)),
        optional(seq(/[eE]/, optional(choice('+', '-')), /\d+(_\d+)*/))
      )
    )),

    string_literal: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        $.interpolation_expression,
        $.string_fragment
      )),
      '"'
    ),

    string_fragment: _ => token.immediate(/[^"\\$]+/),

    escape_sequence: _ => token.immediate(seq(
      '\\',
      choice(
        /[abtnr"'\\]/,
        /x[0-9a-fA-F]{2}/,
        /u[0-9a-fA-F]{4}/,
        /[0-7]{3}/,
        /./
      )
    )),

    interpolation_expression: $ => seq(
      '${',
      $._expression_atom,
      //$.interpolation_content,
      '}'
    ),

    //interpolation_content: $ => repeat1(choice(
    //  $.identifier,
    //  $.number_literal,
    //  $.boolean_literal,
    //  $.null_literal,
    //  $.string_literal,
    //  $.pipeline_operator,
    //  $.logical_operator,
    //  $.assignment_operator,
    //  $.comparison_operator,
    //  $.range_operator,
    //  $.arrow_operator,
    //  $.sigil,
    //  $.punctuation_delimiter,
    //  $.bracket,
    //  $.builtin_command,
    //  $.keyword_declaration,
    //  $.keyword_control,
    //  $.keyword_loop,
    //  $.keyword_async,
    //  alias(token.immediate(/[^}]/), $.interpolation_fragment)
    //)),

    rune_literal: $ => seq(
      "'",
      choice($.escape_sequence, token.immediate(/[^'\\]/)),
      "'"
    ),

    capture_clause: $ => seq('|', $._capture_bindings, '|'),

    _capture_bindings: $ => seq(
      $._capture_binding,
      optional(seq(',', $._capture_binding))
    ),

    _capture_binding: $ => choice($.identifier, '_'),

    builtin_command: _ => choice('echo', 'upper', 'lower'),

    type_identifier: _ => token(prec(1, seq(
      optional(/[?^!]+/),
      /[A-Z][A-Za-z0-9_]*/,
      repeat(seq('.', optional(/[?^!]+/), /[A-Z][A-Za-z0-9_]*/))
    ))),

    pipeline_operator: _ => choice('|', '&'),

    process_operator: _ => token(choice(
      seq(optional(/\d+/), '>', optional('>')),
      '<<'
    )),

    logical_operator: _ => choice('and', 'or', '!'),

    comparison_operator: _ => token(choice(
      '==',
      '!=',
      '<=',
      '>=',
      '<',
      '>'
    )),

    assignment_operator: _ => token('='),

    range_operator: _ => choice('..', '...'),

    arrow_operator: _ => choice('->', '=>'),

    sigil: _ => choice('?', '^', '!'),

    bracket: _ => choice('(', ')', '[', ']', '{', '}'),

    punctuation_delimiter: _ => choice(',', ':', '.'),

    command_head: _ => token(prec(-1, seq(
      choice(/[A-Za-z_]/, '.', '/'),
      repeat(choice(/[A-Za-z0-9_]/, /[./-]/))
    ))),

    identifier: _ => token(/[A-Za-z_][A-Za-z0-9_-]*/),

    stage_separator: _ => ';',

    // heredoc_fence: $ => seq('<<', field('tag', $.identifier)),

    documentation_comment: $ => prec(1, choice(
      /\/\/\/[^\n]*/,
      seq('/**', repeat(choice($.block_comment, /[^*]/, /\*[^/]/)), '*/')
    )),

    line_comment: _ => token(choice(
      /#[^\n]*/,
      /\/\/[^\n]*/
    )),

    block_comment: $ => seq(
      '/*',
      repeat(choice($.block_comment, /[^*]/, /\*[^/]/)),
      '*/'
    ),
  },
});
