//
// This package simply wrapper the underline buffer to play as a
// fixed-size buffer.
//
// IMPORTANT!!! The read position is the position where user would
// read something; while the write position is the position where user
// has already write the content, in other words, when write, the write
// position would forward one step.
//
// 约定：读本位置（即rdptr所在的位置），写下一个位置（即wrptr的下一个位置可写）
//      对于用户来说，无须关注这些细节，只需按照相应的逻辑调整WrPtr和RdPtr即可。
//
// TODO: 当按不规则长度的字节重复写入缓冲区，同时不进行任何读取时，返回的可写区域未知。
//       读取也有类似的问题。
//
//Author: neutrous

package ringbuf

import (
	"errors"
	"math"
)

// Buffer is a abstraction of ring byte array.
type Buffer struct {
	rdptr  int
	wrptr  int
	len    int
	wrfull bool
	impl   []byte
}

// New uses the len to initialize a Buffer instance.
func New(length int) *Buffer {
	if length < 0 {
		return nil
	}
	// -1 indicates the initial state.
	return &Buffer{-1, -1, length, false, make([]byte, length)}
}

// Len returns the underly buffer's fixed length.
func (obj *Buffer) Len() int {
	return len(obj.impl)
}

// Writable returns a possibly 2 elements 2 devison byte array. User
// should check the length of returned value. Change the return
// value's content would also change the Buffer's data.
func (obj *Buffer) Writable() ([][]byte, error) {
	var retval [][]byte
	if obj.len == 0 {
		return retval, errors.New("0 length buffer.")
	}

	if is, _ := obj.Empty(); is {
		// Empty buffer
		retval = make([][]byte, 1)
		retval[0] = obj.impl[:]
		return retval, nil
	}

	if is, _ := obj.Full(); is {
		return nil, nil
	}

	// Common situation
	var rdPos uint
	if obj.rdptr == -1 {
		rdPos = 0
	} else {
		rdPos = uint(obj.rdptr)
	}

	wrPos := uint(obj.wrptr) % uint(obj.len)

	wrlen := uint(math.Abs(float64((rdPos - wrPos) % uint(obj.len))))

	lastIdx := wrlen + wrPos%uint(obj.len)
	if lastIdx > uint(obj.len) {
		// One slice is not adequate
		retval = make([][]byte, 2)
		retval[0] = obj.impl[wrPos:]
		retval[1] = obj.impl[:lastIdx-uint(obj.len)]
	} else {
		retval = make([][]byte, 1)
		retval[0] = obj.impl[wrPos:lastIdx]
	}
	return retval, nil
}

// WrPtr adjust the write (forward) position of underly buffer.
func (obj *Buffer) WrPtr(bytes uint) error {
	if obj.len == 0 {
		return errors.New("0 Length buffer shouldn't writable.")
	}

	empty, _ := obj.Empty()
	overlap := (int(math.Abs(float64(bytes))) > obj.len) //||
		//(obj.rdptr != -1 && (((bytes + uint(obj.wrptr)) % uint(obj.len)) > uint(obj.rdptr))))
	if overlap {
		return errors.New("Overlapped writing.")
	}

	if bytes == 0 {
		return nil
	}

	if bytes == uint(obj.len) && empty {
		// Directly writing full!!! On this situation, we don't need
		// to adjust the position of writer position.
		obj.wrfull = true
		if obj.wrptr == -1 {
			obj.wrptr = obj.len
		}
		if obj.rdptr == -1 {
			obj.rdptr = 0
		}
		return nil
	}

	if obj.wrptr == -1 {
		obj.wrptr = 0
	}

	obj.wrptr = int((uint(obj.wrptr) + bytes) % uint(obj.len))
	if obj.wrptr == obj.rdptr {
		obj.wrfull = true
	} else {
		obj.wrfull = false
	}
	return nil
}

// RdPtr adjusts the read position (forward) of underly buffer.
func (obj *Buffer) RdPtr(bytes uint) error {
	if obj.len == 0 {
		return errors.New("0 Length buffer shouldn't be readable.")
	}

	empty, _ := obj.Empty()
	empty = empty && (obj.wrptr != -1 && obj.rdptr != -1)
	overflow := int(math.Abs(float64(bytes))) > obj.len || empty// ||
		//(obj.wrptr != -1 && (((bytes + uint(obj.rdptr)) % uint(obj.len)) > uint(obj.wrptr)))
	if overflow {
		return errors.New("Overflow reading.")
	}

	if bytes == 0 {
		return nil
	}

	if obj.rdptr == -1 {
		obj.rdptr = 0
	}

	obj.rdptr = int((uint(obj.rdptr) + bytes) % uint(obj.len))
	if ((obj.wrptr + 1) % obj.len) == obj.rdptr && obj.wrfull {
		obj.wrfull = true
	} else {
		obj.wrfull = false
	}
	return nil
}

// Readable returns a copy of the currently readable buffer. Change
// the content of the return value does no matter with underly buffer.
func (obj *Buffer) Readable() ([]byte, error) {
	if obj.len == 0 {
		return nil, errors.New("0 length buffer.")
	}

	if is, _ := obj.Empty(); is {
		return nil, nil
	}

	// Next readable position
	var rdPos, wrPos uint
	if obj.rdptr == -1 {
		rdPos = 0
	} else {
		rdPos = uint(obj.rdptr)
	}

	if is, _ := obj.Full(); is {
		retval := make([]byte, obj.len)
		lastIdx := uint(obj.len) + rdPos%uint(obj.len)
		if lastIdx > uint(obj.len) {
			idx := (uint(len(obj.impl)) - rdPos)%uint(obj.len)
			copy(retval, obj.impl[rdPos%uint(obj.len):])
			copy(retval[idx:], obj.impl[:lastIdx-uint(obj.len)])

		} else {
			copy(retval, obj.impl)
		}
		return retval, nil
	}

	if obj.wrptr == -1 {
		wrPos = 0
	} else {
		wrPos = uint(obj.wrptr)
	}

	rdlen := uint(math.Abs(float64((wrPos - rdPos) % uint(obj.len))))

	lastIdx := rdlen + rdPos%uint(obj.len)

	retval := make([]byte, rdlen)

	if lastIdx > uint(obj.len) {
		// Indicates the last element's index.
		idx := (uint(len(obj.impl)) - rdPos)%uint(obj.len)
		copy(retval, obj.impl[rdPos%uint(obj.len):])
		copy(retval[idx:], obj.impl[:lastIdx-uint(obj.len)])
	} else {
		copy(retval, obj.impl[rdPos%uint(obj.len):lastIdx])
	}
	return retval, nil
}

// Empty indicates the current buffer is empty.
func (obj *Buffer) Empty() (bool, error) {
	if obj.len == 0 {
		return false, errors.New("0 length buffer.")
	}

	if obj.wrfull {
		return false, nil
	}
	
	return obj.wrptr == obj.rdptr, nil
}

// Full indicates the current buffer is full.
func (obj *Buffer) Full() (bool, error) {
	if obj.len == 0 {
		return false, errors.New("0 length buffer.")
	}
	
	if ((obj.wrptr + 1) % obj.len) == obj.rdptr && obj.wrfull {
		return false, nil
	}

	next := (obj.wrptr + 1) % obj.len
	return obj.wrfull || next == obj.rdptr, nil
}
















