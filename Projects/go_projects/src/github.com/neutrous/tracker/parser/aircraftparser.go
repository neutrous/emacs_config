// 
// Implementation of AircraftInfo parser.
// 
// Author: neutrous
// 
package parser

import (
	"strings"
	"fmt"
	"strconv"
	
	. "github.com/neutrous/tracker/model"
	"code.google.com/p/goprotobuf/proto"
)

type AircraftInfoParser struct {
}

func (obj *AircraftInfoParser) Parsable(msg []byte) bool {
	return true
}

func (obj *AircraftInfoParser) ParseData(msg []byte) (length int, err error) {

	var aci AircraftInfo
	msgStr := string(msg)
	if strings.Contains(msgStr, "*HQ") &&
		strings.Contains(msgStr, "#") {
		tokens := strings.Split(msgStr, ",")
		
		setProtoFloat64(aci.Longitude, tokens[7])
		setProtoFloat64(aci.Latitude, tokens[5])
		setProtoFloat64(aci.Altitude, tokens[11])
		setProtoFloat64(aci.Speed, tokens[9])
		setProtoFloat64(aci.Direction, tokens[10])
		aci.ID = proto.String(tokens[1])
		
		fmt.Printf("Id: %s, (%f, %f, %f), Speed: %f, Direction: %f\n",
			aci.GetID(), aci.GetLongitude(), aci.GetLatitude(), 
			aci.GetAltitude(), aci.GetSpeed(), aci.GetDirection())
		
	}
	return len(msg), nil
}

func setProtoFloat64(dst *float64, src string) {
	value, err := strconv.ParseFloat(src, 64)
	if err != nil {
		fmt.Println(err)
		return
	}
	dst = proto.Float64(value)
}




















