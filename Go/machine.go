package main

import (
    "io"
	"fmt"
	"log"
    "os"
    "encoding/binary"
)

var memory []uint32

func run() {
    var finger uint32
    var platter uint32
    var code uint8

    for {
        platter = memory[finger]
        code = uint8(platter >> 28)

        fmt.Printf("%d\n", code)
        finger++
    }
}

func load() {
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

        memory = append(memory, binary.BigEndian.Uint32(platter))
    }

	defer file.Close()
}

func main() {
    load()
    run()
}
