package wanf

import (
	"strings"
	"testing"
)

func TestLint2(t *testing.T) {
	testCases := []struct {
		name           string
		input          string
		wantOutput     string
		opts           FormatOptions
		wantErrors     []string
		expectNoChange bool
	}{
		{
			name: "redundant label",
			input: `
database "main" {
	host = "localhost"
}`,
			wantOutput: `
database {
	host = "localhost"
}`,
			wantErrors: []string{"the label \"main\" is redundant"},
		},
		{
			name: "redundant comma",
			input: `
server {
	port = 8080,
	host = "0.0.0.0"
}`,
			wantOutput: `
server {
	host = "0.0.0.0"
	port = 8080
}`,
			wantErrors: []string{"redundant comma"},
		},
		{
			name: "well-formatted file",
			input: `
server "api" {
	host = "localhost"
}
server "grpc" {
	host = "localhost"
}`,
			wantOutput: `server "api" {
	host = "localhost"
}

server "grpc" {
	host = "localhost"
}`,
		},
		{
			name: "streaming style",
			input: `server "b" {
	host = "2"
}
server "a" {
	host = "1"
}`,
			opts: FormatOptions{Style: StyleStreaming, EmptyLines: false},
			wantOutput: `server "b" {
	host = "2"
}
server "a" {
	host = "1"
}`,
		},
		{
			name:       "unclosed block comment",
			input:      `a = 1 /* comment`,
			wantOutput: `a = 1`,
			wantErrors: []string{"unclosed block comment"},
		},
		{
			name: "fmt sorting and newlines",
			input: `
c_kv = 1
a_block {
	z_sub = 1
	a_sub = 2
}
b_map = {[
	key = "val"
]}
`,
			// Note: b_map and c_kv are sorted first, then a_block.
			// Empty lines are added between them because they are top-level.
			// Inside a_block, fields are sorted but have no empty line.
			wantOutput: `c_kv = 1

a_block {
	a_sub = 2
	z_sub = 1
}

b_map = {[
	key = "val",
]}`,
		},
		{
			name: "fmt map and list newlines",
			input: `
c_list = [3, 1, 2]
a_map = {[
    z = 1,
    a = 2,
]}
b_block {
    nested_list = [ "c", "a" ]
    nested_map = {[ b=2, a=1 ]}
}
`,
			wantOutput: `c_list = [
	3,
	1,
	2,
]

a_map = {[
	a = 2,
	z = 1,
]}

b_block {
	nested_list = [
		"c",
		"a",
	]
	nested_map = {[
		a = 1,
		b = 2,
	]}
}`,
		},
		{
			name: "fmt empty block literal",
			input: `
list = [
    {},
    {
    },
]
`,
			wantOutput: `list = [
	{},
	{},
]`,
		},
		{
			name: "fmt with auto-fix for missing comma in map",
			input: `
sList = {[
	item1 = {}
	item2 = {}
]}`,
			opts:       FormatOptions{Style: StyleBlockSorted, EmptyLines: true},
			wantErrors: []string{"missing comma, auto-inserted before IDENT"},
			wantOutput: `sList = {[
	item1 = {},
	item2 = {},
]}`,
		},
		{
			name: "fmt with no sort",
			input: `
c_kv = 1
a_block {
	z_sub = 1
	a_sub = 2
}
b_kv = 2
`,
			opts: FormatOptions{NoSort: true, EmptyLines: true},
			wantOutput: `c_kv = 1

a_block {
	z_sub = 1
	a_sub = 2
}

b_kv = 2`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			if tc.opts.Style == 0 && !tc.opts.EmptyLines {
				tc.opts.Style = StyleBlockSorted
				tc.opts.EmptyLines = true
			}
			program, errs := Lint([]byte(tc.input))

			if len(tc.wantErrors) > 0 {
				if len(errs) != len(tc.wantErrors) {
					t.Fatalf("expected %d errors, got %d. Errors: %v", len(tc.wantErrors), len(errs), errs)
				}
				for i, wantErr := range tc.wantErrors {
					if !strings.Contains(errs[i].Error(), wantErr) {
						t.Errorf("expected error %q to contain %q", errs[i], wantErr)
					}
				}
				// If we only wanted to check for errors and not the output, return.
				if tc.wantOutput == "" {
					return
				}
			} else if len(errs) > 0 {
				t.Fatalf("expected no errors, got: %v", errs)
			}

			formattedBytes := Format(program, tc.opts)

			if tc.expectNoChange {
				if string(formattedBytes) != tc.input {
					t.Errorf("expected no change, but got diff:\n--- want\n%s\n--- got\n%s", tc.input, string(formattedBytes))
				}
			} else {
				// Normalize whitespace for comparison
				got := strings.TrimSpace(string(formattedBytes))
				want := strings.TrimSpace(tc.wantOutput)
				if got != want {
					t.Errorf("output mismatch:\n--- want\n%s\n--- got\n%s", want, got)
				}
			}
		})
	}
}
