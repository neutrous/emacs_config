//
// Testing for ringbuf.Buffer
//
package ringbuf_test

import (
	"bytes"
	"github.com/neutrous/tracker/ringbuf"
	"math/rand"
	"testing"
	"time"
	"fmt"
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
	
	buf1024 = ringbuf.New(1024)
	buf1024.WrPtr(1)
	if writableLength(buf1024) != 1023 {
		t.Errorf("After set the writer position, the writable length should be 1023, "+
			"while got %v\n", writableLength(buf1024))
	}
	
	if readableLength(buf1024) != 1 {
		t.Errorf("After set the writer position, the writable length should be 1, "+
			"while got %v\n", readableLength(buf1024))
	}

	buf1024 = ringbuf.New(1024)

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

	// We have read 100 bytes
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

func TestReadWrite(t *testing.T) {
	buf1024 := ringbuf.New(1024)

	buf1024.WrPtr(100)
	if writableLength(buf1024) != 924 {
		t.Errorf("After set the write posistion, the writable length should be 924, "+
			"while got %v\n", writableLength(buf1024))
	}

	if readableLength(buf1024) != 100 {
		t.Errorf("After set the write position, the readable length shoudl be 100, "+
			"while got %v\n", readableLength(buf1024))
	}

	buf1024.RdPtr(100)
	if is, _ := buf1024.Empty(); !is {
		t.Errorf("Currently, the buffer should be empty. However, it's not.\n")
	}

	if writableLength(buf1024) != 1024 {
		t.Errorf("After set the read position, the writable length should be 1024, "+
			"while got %v\n", writableLength(buf1024))
	}

	if readableLength(buf1024) != 0 {
		t.Errorf("After set the read position, the readable length should be 0, "+
			"while got %v\n", readableLength(buf1024))
	}
}

func TestReadWriteLoop(t *testing.T) {
	buf1024 := ringbuf.New(1024)

	for idx := 0; idx < 100; idx++ {
		err := buf1024.WrPtr(1010)
		if err != nil {
			t.Fatal("WrPtr set failure.", err)
		}

		if writableLength(buf1024) != 14 {
			t.Fatalf("After set the write position, the writable length should be 14, "+
				"while got %v\n", writableLength(buf1024))
		}

		if readableLength(buf1024) != 1010 {
			t.Fatalf("After set the write position, the readable length should be 1010, "+
				"while got %v\n", readableLength(buf1024))
		}

		err = buf1024.RdPtr(1000)
		if err != nil {
			t.Fatal("RdPtr set failure.", err)
		}

		if writableLength(buf1024) != 1014 {
			t.Fatalf("After set the read position, the writable length should be 1014, "+
				"while got %v\n", writableLength(buf1024))
		}

		if readableLength(buf1024) != 10 {
			t.Fatalf("After set the read position, the readable length should be 10, "+
				"while got %v\n", readableLength(buf1024))
		}

		err = buf1024.RdPtr(10)
		if err != nil {
			t.Fatal("RdPtr set failure.", err)
		}

		if writableLength(buf1024) != 1024 {
			t.Fatalf("After set the read position, the writable length should be 1024, "+
				"while got %v\n", writableLength(buf1024))
		}

		if readableLength(buf1024) != 0 {
			t.Fatalf("After set the read position, the readable length should be 0, "+
				"while got %v\n", readableLength(buf1024))
		}
	}
}

func TestDataFectch(t *testing.T) {
	buf256 := ringbuf.New(256)

	// Prepair data
	wrBuffer, err := buf256.Writable()
	if err != nil {
		t.Fatal("Buffer should be empty, while not.", err)
	}

	for _, buf := range wrBuffer {
		for idx := 0; idx < len(buf); idx++ {
			buf[idx] = byte(idx)
		}
	}

	err = buf256.WrPtr(uint(buf256.Len()))
	if err != nil {
		t.Fatal("Should be write full, while not.", err)
	}

	comp := make([]byte, 256)
	for idx := 0; idx < 256; idx++ {
		comp[idx] = byte(idx)
	}

	content, err := buf256.Readable()
	if err != nil {
		t.Fatal("Should be readable, while not.")
	}

	if len(content) != 256 {
		t.Fatal("Currently, the length should be 256, while got ", len(content))
	}

	if 0 != bytes.Compare(content, comp) {
		t.Errorf("Original buffer's conent should be equal with the compared one.\n")
	}

	rand.Seed(time.Now().UnixNano())
	
	err = buf256.RdPtr(1)
	if err != nil {
		t.Fatal("RdPtr set failure", err)
	}
	
	content, err = buf256.Readable()
	if err != nil {
		t.Fatal("Readable data fetch failure.", err)
	}
	
	if len(content) != 255 {
		t.Fatal("Expected readable length shoudl be 255, while got ", len(content))
	}
	
	buf256.WrPtr(1)
	content, err = buf256.Readable()
	if err != nil {
		t.Fatal("Readble data fetch error.", err)
	}
	
	if len(content) != 256 {
		t.Fatal("Expected readable length should be 256, while got \n", len(content))
	}

	// More the one time's loop is trival, because the comparing slice is not a cycle.
	for i := 0; i < 3; i++ {
		idx := rand.Intn(255)
		err := buf256.RdPtr(uint(idx))
		if err != nil {
			t.Fatal("RdPtr set failure.", err)
		}
		// Reset the read status
		buf256.WrPtr(uint(idx))
		content, err = buf256.Readable()
		if err != nil || len(content) != 256 {
			t.Errorf("After reset the status, length should be 256, while got %v\n", len(content))
		}
		content, err = buf256.Readable()
		if err != nil {
			t.Fatal("Readable data fetch failure.", err)
		}
		 
		printByteArray(content)

				// orilen := len(content)
		// complen := len(comp[idx:])
		// if orilen != complen {
		// 	t.Fatalf("Length should be equal, current idx: %v "+
		// 		"expected length %v while %v", idx, complen, orilen)
		// }

		// if 0 != bytes.Compare(content, comp[idx:]) {
		// 	t.Errorf("Content should be equal with compared one.\n")
		// 	printByteArray(content)
		// 	printByteArray(comp[idx:])
		// }
	}
}

func printByteArray(ar []byte) {
	for _, val := range ar {
		fmt.Printf("%v ", val)
	}
	fmt.Println("")
}










