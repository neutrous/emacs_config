//
// Tracker declare the abstraction for information from
// variable hardwares.
//
// Author: neutrous
//
package link

import (
	"errors"
	"fmt"
	"log"
	"net"
	//"time"

	"github.com/neutrous/tracker/parser"
	"github.com/neutrous/tracker/ringbuf"
)

type Address string
type Addresses []Address

const (
	maxBufferSize = 16 * 1024 // The size 16k may be adequate
)

// Link is the abstraction of which the raw data could be transmitted.
type TcpLink struct {
	addr     Address
	bufSize  int
	listener *net.TCPListener
	quit     chan bool

	Parser parser.Parser
}

// NewTcpLink creates a new instance of TcpLink.
func NewTcpLink(addr Address) *TcpLink {
	return &TcpLink{addr: addr, bufSize: maxBufferSize, listener: nil,
		quit: make(chan bool)}
}

func (obj *TcpLink) initLink() (err error) {
	if obj.listener != nil {
		err = errors.New("The TCP link listener has already been initialized.")
		return
	}

	var servAddr *net.TCPAddr
	servAddr, err = net.ResolveTCPAddr("tcp", string(obj.addr))
	if err != nil {
		handleError(err, "Resovling "+string(obj.addr)+" failed.")
		return
	}
	obj.listener, err = net.ListenTCP("tcp", servAddr)
	if err != nil {
		handleError(err, "Listen failed.")
	}
	return
}

func handleError(err error, info string) {
	fmtStr := fmt.Sprintf("Error: %s %s", info, err)
	log.Println(fmtStr)
}

func (obj *TcpLink) StartReceiving(returnWhenFailure bool) {

	// Firstly, initialize the listner
	initErr := obj.initLink()
	if initErr != nil {
		handleError(initErr, "Init TCP linik failure.")
		return
	}

	for {
		conn, err := obj.listener.AcceptTCP()
		if err != nil {
			handleError(err, "AcceptTCP connection failure.")
			if !returnWhenFailure {
				continue
			} else {
				break
			}
		}
		go obj.handleConnect(conn)
	}
}

func (obj *TcpLink) StopReceiving() {
	if obj.listener != nil {
		obj.listener.Close()
	}
	obj.quit <- true
}

func (obj *TcpLink) handleConnect(conn net.Conn) {
	remoteAddr := conn.RemoteAddr().String()
	log.Printf("TcpLink.handleConnect accepted the connection from %s.\n",
		remoteAddr)
	//conn.SetReadDeadline(time.Time{})

	var readBuf [][]byte
	var handleBuf []byte
	var err error
	var rdBytes, hdBytes int // Indicate read bytes and handled bytes
	buffer := ringbuf.New(obj.bufSize)

	for {
		select {
		case <-obj.quit:
			conn.Close()
			return
		default:
			for {
				// 2. Read as many bytes as possible.
				readBuf, err = buffer.Writable()
				if err != nil {
					log.Println("TcpLink.handleConnect error. ", err)
					return
				}

				for idx, _ := range readBuf {
					rdBytes, err = conn.Read(readBuf[idx])
					buffer.WrPtr(uint(rdBytes))

					if err != nil {
						// Skip bad pdu.
						buffer.RdPtr(uint(rdBytes))
						continue
					}

					if rdBytes < len(readBuf[idx]) {
						// Received datas are smaller than capacity.
						break
					}
				}

				handleBuf, err = buffer.Readable()
				if err != nil {
					log.Println("TcpLink.handleConnect read error. ", err)
					return
				}

				// 3. Check whether the received length is adequate to parse.
				if obj.Parser.Parsable(handleBuf) {
					// Currently, received bytes could be handled.
					// The actual length of bytes could handle is: rdBytes + left
					hdBytes, err = obj.Parser.ParseData(handleBuf)
					buffer.RdPtr(uint(hdBytes))
					if err != nil {
						log.Println("TcpLink.handleConnect parse data failure. ", err)
						continue
					}
				}
			}
		}
	}
}

// func skipBadPdu(rdPtr *int, badBytes int) {
// 	*rdPtr += badBytes
// }
