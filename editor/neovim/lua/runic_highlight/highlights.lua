local definitions = {
    {
        group = "RunicDeclKeyword",
        capture = "@keyword.declaration.runic",
        link = "Keyword",
        syntax = {
            {
                type = "keyword",
                words = { "let", "mut", "fn" },
            },
        },
    },
    {
        group = "RunicTypeKeyword",
        capture = "@keyword.type.runic",
        link = "Type",
        syntax = {
            {
                type = "keyword",
                words = { "error", "enum", "union" },
            },
        },
    },
    {
        group = "RunicAsyncKeyword",
        capture = "@keyword.coroutine.runic",
        link = "Keyword",
        syntax = {
            {
                type = "keyword",
                words = { "async", "await" },
            },
        },
    },
    {
        group = "RunicControlKeyword",
        capture = "@keyword.control.runic",
        link = "Conditional",
        syntax = {
            {
                type = "keyword",
                words = { "if", "else", "match", "return" },
            },
        },
    },
    {
        group = "RunicLoopKeyword",
        capture = "@keyword.repeat.runic",
        link = "Repeat",
        syntax = {
            {
                type = "keyword",
                words = { "for", "while" },
            },
        },
    },
    {
        group = "RunicImportKeyword",
        capture = "@keyword.import.runic",
        link = "Include",
        syntax = {
            {
                type = "keyword",
                words = { "import", "from" },
            },
        },
    },
    {
        group = "RunicInteropKeyword",
        capture = "@keyword.special.runic",
        link = "Keyword",
        syntax = {
            {
                type = "keyword",
                words = { "bash" },
            },
        },
    },
    {
        group = "RunicErrorKeyword",
        capture = "@keyword.exception.runic",
        link = "Exception",
        syntax = {
            {
                type = "keyword",
                words = { "try", "catch" },
            },
        },
    },
    {
        group = "RunicBooleanLiteral",
        capture = "@constant.builtin.boolean.runic",
        link = "Boolean",
        syntax = {
            {
                type = "keyword",
                words = { "true", "false" },
            },
        },
    },
    {
        group = "RunicNullLiteral",
        capture = "@constant.builtin.nil.runic",
        link = "Constant",
        syntax = {
            {
                type = "keyword",
                words = { "null" },
            },
        },
    },
    {
        group = "RunicNumber",
        capture = "@number.runic",
        link = "Number",
        syntax = {
            {
                type = "match",
                pattern = [[/\v0x[0-9A-Fa-f_]+|\d+(_\d+)*(\.\d+(_\d+)*)?([eE][+-]?\d+)?/]],
            },
        },
    },
    {
        group = "RunicString",
        capture = "@string.runic",
        link = "String",
        syntax = {
            {
                type = "region",
                clauses = {
                    "keepend",
                    [[start=+"+]],
                    [[skip=+\\\\\|\\"+]],
                    [[end=+"+]],
                    "contains=RunicStringEscape,RunicInterpolationExpr",
                },
            },
        },
    },
    {
        group = "RunicStringEscape",
        capture = "@string.escape.runic",
        link = "SpecialChar",
        syntax = {
            {
                type = "match",
                pattern = [[/\\\%([0-7]\{3}\|x\x\{2}\|u\x\{4}\|.\)/]],
                clauses = { "contained" },
            },
        },
    },
    {
        group = "RunicInterpolationDelimiter",
        capture = "@string.special.symbol.runic",
        link = "Delimiter",
    },
    {
        group = "RunicInterpolationExpr",
        capture = "@embedded.runic",
        link = "SpecialChar",
        syntax = {
            {
                type = "region",
                clauses = {
                    "contained",
                    "keepend",
                    "matchgroup=RunicInterpolationDelimiter",
                    [[start=+${+]],
                    [[end=+}+]],
                    "contains=TOP",
                },
            },
        },
    },
    {
        group = "RunicRuneLiteral",
        capture = "@character.runic",
        link = "Character",
        syntax = {
            {
                type = "match",
                pattern = [[/'\%(\\.\|[^\\]\)'/]],
            },
        },
    },
    {
        group = "RunicCaptureBinding",
        capture = "@keyword.capture.runic",
        link = "Special",
        syntax = {
            {
                type = "region",
                clauses = {
                    "oneline",
                    "keepend",
                    [[start=+|\s*+]],
                    [[end=+|+]],
                },
            },
        },
    },
    {
        group = "RunicBuiltinCmd",
        capture = "@function.builtin.runic",
        link = "Function",
        syntax = {
            {
                type = "keyword",
                words = { "echo", "upper", "lower" },
            },
        },
    },
    {
        group = "RunicTypeName",
        capture = "@type.identifier.runic",
        link = "Type",
        syntax = {
            {
                type = "match",
                pattern = [[/\%(:\|->\)\s*\zs\%([?^!]*\h\w*\%(\.\h\w*\)*\)/]],
                clauses = { "contains=RunicSigil" },
            },
            {
                type = "match",
                pattern = [[/\%(^\|\W\)\zs[?^!]*\u\w*\%(\.\u\w*\)*/]],
                clauses = { "contains=RunicSigil" },
            },
        },
    },
    {
        group = "RunicPipelineOperator",
        capture = "@operator.pipeline.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/\%(^\|[^|]\)\zs|\ze\%([^|]\|$\)\|\%(^\|[^&]\)\zs&\ze\%([^&]\|$\)/]],
            },
        },
    },
    {
        group = "RunicProcessOperator",
        capture = "@operator.process.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/\d\?>\%(>\)\?\|>>\|<</]],
            },
        },
    },
    {
        group = "RunicLogicOperator",
        capture = "@operator.logical.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/&&\|||!\%(=\)\@!/]],
            },
        },
    },
    {
        group = "RunicAssignmentOperator",
        capture = "@operator.assignment.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/=\%(=\|>\)\@!/]],
            },
        },
    },
    {
        group = "RunicComparisonOperator",
        capture = "@operator.comparison.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/==\|!=\|<=\|>=\|<\%(=\|<\)\@!\|>\%(=\|>\)\@!/]],
            },
        },
    },
    {
        group = "RunicRangeOperator",
        capture = "@operator.range.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/\.\.\.\|\.\./]],
            },
        },
    },
    {
        group = "RunicArrowOperator",
        capture = "@operator.arrow.runic",
        link = "Operator",
        syntax = {
            {
                type = "match",
                pattern = [[/=>\|->/]],
            },
        },
    },
    {
        group = "RunicSigil",
        capture = "@operator.sigil.runic",
        link = "SpecialChar",
        syntax = {
            {
                type = "match",
                pattern = [[/[?^]/]],
                clauses = { "contained", "containedin=RunicTypeName" },
            },
            {
                type = "match",
                pattern = [[/!\ze\h/]],
                clauses = { "contained", "containedin=RunicTypeName" },
            },
        },
    },
    {
        group = "RunicDelimiter",
        capture = "@punctuation.bracket.runic",
        link = "Delimiter",
        syntax = {
            {
                type = "match",
                pattern = [[/[\[\]{}()]/]],
            },
        },
    },
    {
        group = "RunicComma",
        capture = "@punctuation.delimiter.runic",
        link = "Delimiter",
        syntax = {
            {
                type = "match",
                pattern = [[/[,:.]/]],
            },
        },
    },
    {
        group = "RunicLineComment",
        capture = "@comment.line.runic",
        link = "Comment",
        syntax = {
            {
                type = "match",
                pattern = [[/#.*$/]],
                clauses = { "display" },
            },
            {
                type = "match",
                pattern = [[/\/\/.*$/]],
                clauses = { "display" },
            },
            {
                type = "match",
                pattern = [[/\%(^\|[^/]\)\zs//\%(/\)\@!.*$/]],
                clauses = { "display" },
            },
        },
    },
    {
        group = "RunicDocComment",
        capture = "@comment.documentation.runic",
        link = "SpecialComment",
        syntax = {
            {
                type = "match",
                pattern = [[/\/\/\/.*$/]],
                clauses = { "display" },
            },
            {
                type = "region",
                clauses = {
                    "keepend",
                    "extend",
                    [[start=+/\*\*+]],
                    [[end=+\*/+]],
                },
            },
        },
    },
    {
        group = "RunicBlockComment",
        capture = "@comment.block.runic",
        link = "Comment",
        syntax = {
            {
                type = "region",
                clauses = {
                    "keepend",
                    "extend",
                    [[start=+/\*\%(\*\)\@!+]],
                    [[end=+\*/+]],
                    "contains=RunicBlockComment",
                },
            },
        },
    },
    {
        group = "RunicCommandHead",
        capture = "@function.call.runic",
        link = "Function",
        syntax = {
            {
                type = "match",
                pattern = [[/\%(^\|\%(|\|&\|;\|\%(||\|&&\)\)\)\s*\zs\%([./]\|\h\)\%(\k\|[./-]\)*/]],
                clauses = { "contains=RunicBuiltinCmd" },
            },
        },
    },
    {
        group = "RunicStageSeparator",
        capture = "@punctuation.special.runic",
        link = "Delimiter",
        syntax = {
            {
                type = "match",
                pattern = [[/;/]],
            },
        },
    },
    {
        group = "RunicHereDocFence",
        capture = "@string.special.runic",
        link = "Special",
        syntax = {
            {
                type = "match",
                pattern = [[/<<\h\w*/]],
            },
        },
    },
}

local by_group = {}
for _, def in ipairs(definitions) do
    by_group[def.group] = def
end

return {
    definitions = definitions,
    by_group = by_group,
}
