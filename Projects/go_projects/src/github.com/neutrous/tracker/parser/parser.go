// 
// This package define the abstraction for parse the specified data.
// 
// Author: neutrous
// 
package parser

// Parser is a interface for any specified data type decoder.
type Parser interface {
	// Indicate the packages' max marshaled size on the wired.
	//MaxMarshaledLength() int
	
	// Indicate the specified buffer is adequate to parse.
	Parsable(msg []byte) bool

	// ParseData could handle the specified msg, and return the exact
	// count of bytes it uses. The Link assure that the len(msg) is
	// bigger or equal than MaxMarshaledLength
	ParseData(msg []byte) (length int, err error)
}

