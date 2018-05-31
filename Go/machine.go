package main

import (
	"encoding/binary"
	"fmt"
	"io"
	"log"
	"os"
)

var memory [][]uint32
var reg [8]uint32

type op int

const (
	cmv op = iota
	aix
	aam
	add
	mul
	dvi
	nad
	hlt
	alc
	abd
	otp
	inp
	lod
	ort
)

func run() {
	cycle := 0
	var finger uint32
	var platter uint32
	var code uint8
	var a uint8
	var b uint8
	var c uint8
	var val uint32

	for {
		platter = memory[0][finger]
		code = uint8(platter >> 28)

		if op(code) == ort {
			a = uint8((platter >> 25) & 7)
			val = platter & 0x1ffffff
			// fmt.Printf("%d(%d, %d)\n", code, a, val)
			reg[a] = val
		} else {
			a = uint8((platter >> 6) & 7)
			b = uint8((platter >> 3) & 7)
			c = uint8(platter & 7)

			switch op(code) {
			case cmv: // 00
				if reg[c] != 0 {
					reg[a] = reg[b]
				}
			case aix: // 01
				reg[a] = memory[reg[b]][reg[c]]
			case aam: // 02
				memory[reg[a]][reg[b]] = reg[c]
			case add: // 03
				reg[a] = uint32(int(reg[b]+reg[c]) % (1 << 32))
			case mul: // 04
				reg[a] = uint32(int(reg[b]*reg[c]) % (1 << 32))
			case dvi: // 05
				reg[a] = reg[b] / reg[c]
			case nad: // 06
				reg[a] = (reg[b] & reg[c]) ^ ((1 << 32) - 1)
			case otp: // 10
				fmt.Print(string(reg[c]))
			case lod: // 12
				if reg[b] != 0 {
					copy(memory[0], memory[reg[b]])
				}
				finger = reg[c] - 1
			default:
				fmt.Printf("\n%d(%d, %d, %d, %d)\n", code, a, b, c, val)
				log.Fatal("unknown op: ", op(code))
			}
		}

		finger++
		cycle++
	}
}

func load() {
	memory = append(memory, make([]uint32, 0))
	path := "scrolls/sandmark.umz"

	file, err := os.Open(path)
	if err != nil {
		log.Fatal("Error while opening file", err)
	}

	platter := make([]byte, 4)

	for {
		_, err = file.Read(platter)
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}

		memory[0] = append(memory[0], binary.BigEndian.Uint32(platter))
	}

	defer file.Close()
}

func main() {
	load()
	run()
}
