package main

import (
    "io"
	"fmt"
	"log"
    "os"
    "bytes"
    "encoding/binary"
)

func main() {
	path := "scrolls/sandmark.umz"

	file, err := os.Open(path)
	if err != nil {
		log.Fatal("Error while opening file", err)
    }

    chunk := make([]byte, 4)
    var platter uint32

    for {
        _, err = file.Read(chunk)
        if err == io.EOF {
            break
        } else if err != nil {
            log.Fatal(err)
        }

        buf := bytes.NewReader(chunk)
        binary.Read(buf, binary.BigEndian, &platter)

        fmt.Printf("platter: %d\n", platter)
    }

	defer file.Close()

	fmt.Printf("%s opened\n", path)
}
