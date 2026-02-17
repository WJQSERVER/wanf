package wanf

// lexer is an interface that abstracts the token scanning process.
// Both the original byte-slice-based Lexer and the new stream-based streamLexer
// will satisfy this interface, allowing them to be used by the Parser interchangeably.
type lexer interface {
	NextToken() Token
	IsPersistent() bool
}
