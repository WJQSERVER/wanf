package wanf

import (
	"bytes"
	"strings"
	"testing"
)

func TestStreamEncoder(t *testing.T) {
	type testStruct struct {
		Name    string `wanf:"name"`
		Age     int    `wanf:"age"`
		IsAdmin bool   `wanf:"is_admin,omitempty"`
		Nested  struct {
			City    string   `wanf:"city"`
			Zip     int      `wanf:"zip"`
			Hobbies []string `wanf:"hobbies"`
		} `wanf:"nested"`
	}

	testCases := []struct {
		name       string
		input      any
		opts       []EncoderOption
		wantOutput string
	}{
		{
			name: "Simple struct",
			input: &testStruct{
				Name: "Jules",
				Age:  30,
				Nested: struct {
					City    string   `wanf:"city"`
					Zip     int      `wanf:"zip"`
					Hobbies []string `wanf:"hobbies"`
				}{
					City:    "Paris",
					Zip:     75001,
					Hobbies: []string{"coding", "testing"},
				},
			},
			opts: []EncoderOption{},
			wantOutput: `name = "Jules"
age = 30

nested {
	city = "Paris"
	hobbies = [
		"coding",
		"testing",
	]
	zip = 75001
}
`,
		},
		{
			name: "NoSort option",
			input: &testStruct{
				Name: "Jules",
				Age:  30,
				Nested: struct {
					City    string   `wanf:"city"`
					Zip     int      `wanf:"zip"`
					Hobbies []string `wanf:"hobbies"`
				}{
					City:    "Paris",
					Zip:     75001,
					Hobbies: []string{"coding", "testing"},
				},
			},
			opts: []EncoderOption{func(o *FormatOptions) { o.NoSort = true }},
			wantOutput: `name = "Jules"
age = 30

nested {
	city = "Paris"
	zip = 75001
	hobbies = [
		"coding",
		"testing",
	]
}
`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			enc := NewStreamEncoder(&buf)
			err := enc.Encode(tc.input, tc.opts...)

			if err != nil {
				t.Fatalf("Encode() error = %v", err)
			}

			got := strings.TrimSpace(buf.String())
			want := strings.TrimSpace(tc.wantOutput)

			if got != want {
				t.Errorf("output mismatch:\n--- want\n%s\n--- got\n%s", want, got)
			}
		})
	}
}
