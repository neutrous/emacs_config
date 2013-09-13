// tcpClient project main.go
package main

import (
	"fmt"
	"net"
	"os"
	"strconv"
	"time"
)

func main() {
	tcpAddr, err := net.ResolveTCPAddr("tcp4", "127.0.0.1:54322")
	checkError(err)
	conn, err := net.DialTCP("tcp", nil, tcpAddr)
	checkError(err)

	longitude := 11319.2
	latitude := 2254.6
	direction := 0

	count := 0
	//_, err = conn.Write([]byte("*HQ,7893267624,V1,020110,V,3918.0000,N,07330.0000,E,0.00,000,224,181212,FFFFFBFF#"))"*HQ,7893267624,V1,020110,V,2254.6000,N,11319.2000,E,0.00,000,224,181212,FFFFFBFF#"
	for {
		loTmp := strconv.FormatFloat(longitude, 'f', 4, 64)
		laTmp := strconv.FormatFloat(latitude, 'f', 4, 64)
		dirTmp := strconv.Itoa(direction)
		if count%2 == 0 {
			str := "*HQ,7893267624,V1,020110,V," + laTmp + ",N," + loTmp + ",E,0.00," + dirTmp + ",224,181212,FFFFFBFF#"

			fmt.Println(str)
			_, err = conn.Write([]byte(str))
			longitude += 0.003
			latitude += 0.013
			if longitude > 12000 {
				longitude = 10000
			}
			if latitude > 7000 {
				latitude = 2000
			}
			if longitude-float64(int(longitude/100)*100) >= 60.0 {
				longitude = float64(int((longitude + 100) / 100))

			}
			if latitude-float64(int(latitude/100)*100) >= 60.0 {
				latitude = float64(int((latitude + 100) / 100))

			}

		} else {
			_, err = conn.Write([]byte("*HQ,7893267623,V1,020110,V,2254.7000,N,11319.3000,E,0.00,000,224,181212,FFFFFBFF#"))
		}

		checkError(err)
		time.Sleep(2 * time.Second)
		count++
		direction++
		if direction > 359 {
			direction = 0
		}
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
