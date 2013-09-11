//
// This package simply wrapper the underline buffer to play as a
// fixed-size buffer.
//
// Author: neutrous

package ringbuf

import (
	"math"
)

// Buffer is a abstraction of ring byte array.
type Buffer struct {
	rdptr uint
	wrptr uint
	len   uint
	impl  []byte
}

// New uses the len to initialize a Buffer instance.
func New(len uint) *Buffer {
	return &Buffer{0, 0, len, make([]byte, len)}
}

// Len returns the underly buffer's fixed length.
func (obj *Buffer) Len() int {
	return len(obj.impl)
}

// Writable returns a possibly 2 elements 2 devison byte array. User
// should check the length of returned value. Change the return
// value's content would also change the Buffer's data.
func (obj *Buffer) Writable() [][]byte {
	var retval [][]byte
	if obj.rdptr == obj.wrptr {
		// Empty buffer
		retval = make([][]byte, 1)
		retval[0] = obj.impl[:]
		return retval
	}

	// Common situation
	wrlen := uint(math.Abs(float64(obj.rdptr - obj.wrptr%obj.len)))
	lastIdx := wrlen + obj.wrptr%obj.len
	if lastIdx > obj.len {
		// One slice is not adequate
		retval = make([][]byte, 2)
		retval[0] = obj.impl[obj.wrptr:]
		retval[1] = obj.impl[:lastIdx-obj.len]
	} else {
		retval = make([][]byte, 1)
		retval[0] = obj.impl[obj.wrptr:lastIdx]
	}
	return retval
}

// WrPtr adjust the write position of underly buffer.
func (obj *Buffer) WrPtr(n int) {
	// if (obj.wrptr + n) % obj.len >= obj.rdptr {
	// 	return errors.New("After forward/backward n elements, overlapped occuring.")
	// }
	obj.wrptr = uint(int(obj.wrptr)+n) % obj.len
}

// RdPtr adjusts the read position of underly buffer.
func (obj *Buffer) RdPtr(n int) {
	obj.rdptr = uint(int(obj.rdptr)+n) % obj.len
}

// Readable returns a copy of the currently readable buffer. Change
// the content of the return value does no matter with underly buffer.
func (obj *Buffer) Readable() []byte {
	rdlen := uint(math.Abs(float64((obj.wrptr - obj.rdptr) % obj.len)))
	lastIdx := rdlen + obj.rdptr%obj.len

	retval := make([]byte, rdlen)

	if lastIdx > obj.len {
		// Indicates the last element's index.
		idx := uint(len(obj.impl)) - obj.rdptr%obj.len
		copy(retval, obj.impl[obj.rdptr%obj.len:])
		copy(retval[idx:], obj.impl[:lastIdx-obj.len])
	} else {
		copy(retval, obj.impl[obj.rdptr%obj.len:lastIdx])
	}
	return retval
}

// func main() {

// 	buffer := New(1024)
// 	println("Buffer Length: ", buffer.Len())

// 	wrbuffer := buffer.Writable()
// 	count := 0
// 	for _, value := range wrbuffer {
// 		count += len(value)
// 		for idx := 0; idx < len(value); idx++ {
// 			value[idx] = byte(idx)
// 		}
// 	}

// 	println("writable count: ", count)
// 	println("readable count: ", len(buffer.Readable()))

// 	buffer.WrPtr(4)
// 	buffer.RdPtr(8)

// 	wrbuffer = buffer.Writable()
// 	count = 0
// 	for _, value := range wrbuffer {
// 		count += len(value)
// 	}
// 	println("writable count: ", count)
// 	println("readable count: ", len(buffer.Readable()))

// 	testBuffer := make([]byte, 1020)
// 	for idx, value := 0, 8; idx < 1020; idx++ {
// 		testBuffer[idx] = byte(value)
// 		value++
// 	}

// 	if 0 == bytes.Compare(buffer.Readable(), testBuffer) {
// 		println("Equal")
// 	}

// }
