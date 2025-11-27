package tree_sitter_runic_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_runic "github.com/lumorsunil/runic/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_runic.Language())
	if language == nil {
		t.Errorf("Error loading Runic grammar")
	}
}
