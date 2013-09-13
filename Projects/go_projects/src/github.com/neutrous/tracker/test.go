// 
// A simple test example for tcplink tracker.
// 
// Author: neutrous
// 
package main 

import (
	"github.com/neutrous/tracker/link"
	"github.com/neutrous/tracker/parser"
)

func main() {
	tcpLink := link.NewTcpLink("127.0.0.1:54322")
	tcpLink.Parser = &parser.AircraftInfoParser{}
	
	tcpLink.StartReceiving(true)
}










