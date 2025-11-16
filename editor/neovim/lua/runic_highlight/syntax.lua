local highlights = require("runic_highlight.highlights")

local M = {}

local comment_annotation_pattern =
[[/\v<(TODO|FIXME|BUG|HACK|NOTE|XXX|OPTIMIZE|WARN(ING)?|NB)>/]]
local comment_groups = {
    "RunicLineComment",
    "RunicDocComment",
    "RunicBlockComment",
}

local function link_group(def)
    if not def.link then
        return
    end
    vim.cmd(("highlight default link %s %s"):format(def.group, def.link))
end

local function join_clauses(clauses)
    if not clauses or #clauses == 0 then
        return ""
    end
    return table.concat(clauses, " ") .. " "
end

local function escape_unescaped_slashes(pattern)
    local escaped = {}
    local i = 1
    local len = #pattern
    while i <= len do
        local char = pattern:sub(i, i)
        if char == "\\" and i < len then
            table.insert(escaped, pattern:sub(i, i + 1))
            i = i + 2
        elseif char == "/" then
            table.insert(escaped, "\\/")
            i = i + 1
        else
            table.insert(escaped, char)
            i = i + 1
        end
    end
    return table.concat(escaped)
end

local function normalize_pattern(pattern)
    if not pattern or #pattern < 2 then
        return pattern
    end
    local delimiter = pattern:sub(1, 1)
    if delimiter ~= pattern:sub(-1) then
        return pattern
    end
    if delimiter ~= "/" then
        return pattern
    end
    local body = pattern:sub(2, -2)
    return "/" .. escape_unescaped_slashes(body) .. "/"
end

local function define_keyword(def, spec)
    if not spec.words or #spec.words == 0 then
        return
    end
    local clause = join_clauses(spec.clauses)
    vim.cmd(
        ("syntax keyword %s %s%s"):format(
            def.group,
            clause,
            table.concat(spec.words, " ")
        )
    )
end

local function define_match(def, spec)
    if not spec.pattern then
        return
    end
    local pattern = normalize_pattern(spec.pattern)
    local clause = join_clauses(spec.clauses)
    vim.cmd(("syntax match %s %s%s"):format(def.group, clause, pattern))
end

local function define_region(def, spec)
    if not spec.clauses or #spec.clauses == 0 then
        return
    end
    vim.cmd(
        ("syntax region %s %s"):format(def.group, table.concat(spec.clauses, " "))
    )
end

local function define_syntax(def)
    if not def.syntax then
        return
    end
    for _, spec in ipairs(def.syntax) do
        if spec.type == "keyword" then
            define_keyword(def, spec)
        elseif spec.type == "match" then
            define_match(def, spec)
        elseif spec.type == "region" then
            define_region(def, spec)
        end
    end
end

local function highlight_comment_annotations()
    local containers = {}
    for _, group in ipairs(comment_groups) do
        if highlights.by_group and highlights.by_group[group] then
            table.insert(containers, group)
        end
    end
    if #containers == 0 then
        return
    end
    vim.cmd(
        ("syntax match Todo %s contained containedin=%s"):format(
            comment_annotation_pattern,
            table.concat(containers, ",")
        )
    )
end

function M.setup()
    for _, def in ipairs(highlights.definitions) do
        link_group(def)
        define_syntax(def)
    end

    highlight_comment_annotations()
end

return M
