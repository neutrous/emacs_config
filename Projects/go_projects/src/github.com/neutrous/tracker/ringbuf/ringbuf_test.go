//
// Testing for ringbuf.Buffer
//
package ringbuf_test

import (
	"github.com/neutrous/tracker/ringbuf"
	"testing"
)

var buf1024 = ringbuf.New(1024)
var buf0 = ringbuf.New(0)

func writableLength(buf *ringbuf.Buffer) int {
	wrBuffer, _ := buf.Writable()
	count := 0
	for _, value := range wrBuffer {
		count += len(value)
	}
	return count
}

func readableLength(buf *ringbuf.Buffer) int {
	rdBuffer, _ := buf.Readable()
	return len(rdBuffer)
}

func TestLen(t *testing.T) {
	if buf1024.Len() != 1024 {
		t.Errorf("Buf length should be 1024, while got %v\n", buf1024.Len())
	}

	if buf0.Len() != 0 {
		t.Errorf("Buf length should be 0, while got %v\n", buf0.Len())
	}
}

func TestWrPtr(t *testing.T) {
	if writableLength(buf1024) != 1024 {
		t.Errorf("Buf writable length should be 1024, while got %v\n", writableLength(buf1024))
	}

	if writableLength(buf0) != 0 {
		t.Errorf("Buf writable length should be 0, while got %v\n", writableLength(buf0))
	}

	buf1024.WrPtr(1024)
	if writableLength(buf1024) != 0 {
		t.Errorf("After set the writer position, the wriable length should be 0, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	if readableLength(buf1024) != 1024 {
		t.Errorf("After set the writer postition, the readable length should be 1024, "+
			"while got %v", readableLength(buf1024))
	}

	buf1024.WrPtr(1023)
	if writableLength(buf1024) != 1 {
		t.Errorf("After set the writer position, the writable length should be 1, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	if readableLength(buf1024) != 1023 {
		t.Errorf("After set the writer position, the readable length should be 1023, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	buf1024 = ringbuf.New(1024)

	buf1024.WrPtr(1022)
	if writableLength(buf1024) != 2 {
		t.Errorf("After set the writer position, the writable length should be 2, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	if readableLength(buf1024) != 1022 {
		t.Errorf("After set the writer position, the readable length should be 1022, "+
			"while got %v\n", readableLength(buf1024))
	}

	buf1024.WrPtr(0)
	if writableLength(buf1024) != 2 {
		t.Errorf("After set the writer position, the writable length should be 2, "+
			"while got %v", writableLength(buf1024))
	}
	
	if readableLength(buf1024) != 1022 {
		t.Errorf("After set the writer position, the readable length should be 1022, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	buf1024 = ringbuf.New(1024)
	
	buf1024.WrPtr(100)

	if writableLength(buf1024) != 924 {
		t.Errorf("After set the writer position, the writable length should be 924, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	if readableLength(buf1024) != 100 {
		t.Errorf("After set the writer position, the readable length should be 100, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	buf1024.WrPtr(0)
	if writableLength(buf1024) != 924 {
		t.Errorf("After set the writer position, the writable length should be 924, "+
			"while got %v\n", writableLength(buf1024))
	}

	if readableLength(buf1024) != 100 {
		t.Errorf("After set the writer position, the readable length should be 100, "+
			"while got %v\n", readableLength(buf1024))
	}

	err := buf0.WrPtr(111)
	if err == nil {
		t.Error("No matter how set, the 0 length buffer shouldn't writable, "+
			"while got %v\n", writableLength(buf0))
	}
}

func TestRdPtr(t *testing.T) {
	buf1024 := ringbuf.New(1024)
	
	// Prepare the readable buffer.
	buf1024.WrPtr(uint(buf1024.Len()))
	
	if readableLength(buf1024) != 1024 {
		t.Errorf("There should be 1024 bytes length readable, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	// We have read 101 bytes
	buf1024.RdPtr(100)
	if readableLength(buf1024) != 924 {
		t.Errorf("After set the read position, the readable length should be 924, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	if writableLength(buf1024) != 100 {
		t.Errorf("After set the write position, the writable length should be 100, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	buf1024.RdPtr(300)
	if readableLength(buf1024) != 624 {
		t.Errorf("After set the read position, the readable length should be 624, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	if writableLength(buf1024) != 400 {
		t.Errorf("After set the read position, the readble length should be 400, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	buf1024.RdPtr(0)
	if readableLength(buf1024) != 624 {
		t.Errorf("After set the read position, the readable length should be 624, "+
			"while got %v\n", readableLength(buf1024))
	}
	
	if writableLength(buf1024) != 400 {
		t.Errorf("After set the read position, the writable length should be 400, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	err := buf0.RdPtr(124)
	if err == nil {
		t.Errorf("0 length size buffer shouldn't be readable.\n")
	}
}
