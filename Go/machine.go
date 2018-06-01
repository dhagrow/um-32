package main

import (
	"encoding/binary"
	"fmt"
	"io"
	"log"
	"os"
)

/*
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
char getch(){
    char ch = 0;
    struct termios old = {0};
    fflush(stdout);
    if( tcgetattr(0, &old) < 0 ) perror("tcsetattr()");
    old.c_lflag &= ~ICANON;
    old.c_lflag &= ~ECHO;
    old.c_cc[VMIN] = 1;
    old.c_cc[VTIME] = 0;
    if( tcsetattr(0, TCSANOW, &old) < 0 ) perror("tcsetattr ICANON");
    if( read(0, &ch,1) < 0 ) perror("read()");
    old.c_lflag |= ICANON;
    old.c_lflag |= ECHO;
    if(tcsetattr(0, TCSADRAIN, &old) < 0) perror("tcsetattr ~ICANON");
    return ch;
}
*/
import "C" // poor go :/

var memory [][]uint32
var abandonedIndexes []uint32
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

func state(finger uint32, cycle int, code uint8, a uint8, b uint8, c uint8, val uint32) {
	fmt.Printf("\nSTOP at %d (cycle %d)\n", finger, cycle)
	fmt.Printf("op|%d(%d, %d, %d, %d)\n", code, a, b, c, val)
	fmt.Printf("reg %v\n", reg)
	fmt.Printf("memory: {0: %d}\n", len(memory[0]))
}

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
		if finger >= uint32(len(memory[0])) {
			state(finger, cycle, code, a, b, c, val)
			log.Fatal("out of range: ", finger)
		}
		platter = memory[0][finger]
		code = uint8(platter >> 28)

		if op(code) == ort {
			a = uint8((platter >> 25) & 7)
			val = platter & 0x1ffffff
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
			case hlt: // 07
				return
			case alc: // 08
				var index uint32
				var newArray = make([]uint32, reg[c])

				if len(abandonedIndexes) > 0 {
					index = abandonedIndexes[0]
					abandonedIndexes = abandonedIndexes[1:]
					memory[index] = newArray
				} else {
					index = uint32(len(memory))
					memory = append(memory, newArray)
				}
				reg[b] = index
			case abd: // 09
				memory[reg[c]] = nil
				abandonedIndexes = append(abandonedIndexes, reg[c])
			case otp: // 10
				fmt.Print(string(reg[c]))
			case inp: // 11
				reg[c] = uint32(C.getch())
			case lod: // 12
				if reg[b] != 0 {
					// have to expand the dst array first
					memory[0] = make([]uint32, len(memory[reg[b]]))
					copy(memory[0], memory[reg[b]])
				}
				finger = reg[c] - 1
			default:
				state(finger, cycle, code, a, b, c, val)
				log.Fatal("unknown op: ", op(code))
			}
		}

		finger++
		cycle++
	}
}

func load(path string) {
	memory = append(memory, make([]uint32, 0))

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
	if len(os.Args) < 2 {
		fmt.Println("usage: machine <source>")
		return
	}
	load(os.Args[1])
	run()
}
