open System.IO

type Op =
    | cmv=0
    | aix=1
    | aam=2
    | add=3
    | mul=4
    | dvi=5
    | nad=6
    | hlt=7
    | alc=8
    | abd=9
    | out=10
    | inp=11
    | lod=12
    | ort=13

let mutable memory = new ResizeArray<ResizeArray<uint32>>()
let mutable reg: uint32 array = Array.zeroCreate 8

let load filename =
    let mutable program = new ResizeArray<uint32>()
    let fp = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
    let reader = new BinaryReader(fp)
    try
        while true do
            let byte = reader.ReadUInt32()
            program.Add byte
    with
    | :? System.IO.EndOfStreamException -> ()

    memory.Add program

let run () =
    let finger = ref 0
    let mutable stop = false

    let mutable platter = 0ul
    let mutable code = 0uy
    let mutable a = 0uy
    let mutable b = 0uy
    let mutable c = 0uy
    let mutable value = 0ul

    while not stop && !finger < memory.[0].Count do
        platter <- memory.[0].[!finger]
        code <- uint8 platter >>> 28

        printfn "code: %u" code

        match enum<Op>(int32 code) with
        | Op.ort ->
            a <- (uint8 (platter >>> 25)) &&& 7uy
            value <- platter &&& 0x1fffffful
            reg.[int a] <- value
        | _ ->
            a <- (uint8 (platter >>> 6)) &&& 7uy
            b <- (uint8 (platter >>> 3)) &&& 7uy
            c <- (uint8 platter) &&& 7uy

            match enum<Op>(int32 code) with
            | Op.cmv -> if reg.[int c] <> 0ul then reg.[int a] <- reg.[int b]
            | Op.add -> reg.[int a] <- reg.[int b] + reg.[int c]
            | Op.lod ->
                if reg.[int b] <> 0ul then
                    memory.[0] <- new ResizeArray<uint32>(memory.[int reg.[int b]])
                finger := reg.[int c]
            | _ ->
                printfn "unknown code: %u" code
                stop <- true

        incr finger

load @"scrolls/sandmark.umz"
run()
