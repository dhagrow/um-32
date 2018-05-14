open System.IO

type Op =
    | cmv=0 | aix=1 | aam=2 | add=3 | mul=4 | dvi=5 | nad=6 | hlt=7 | alc=8
    | abd=9 | out=10 | inp=11 | lod=12 | ort=13

let mutable memory = new ResizeArray<ResizeArray<uint32>>()
let mutable reg: uint32 array = Array.zeroCreate 8

let load filename =
    let mutable program = new ResizeArray<uint32>()
    let fp = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
    let reader = new BinaryReader(fp)
    try
        while true do
            let platter = reader.ReadInt32() |> System.Net.IPAddress.HostToNetworkOrder
            program.Add (uint32 platter)
    with
    | :? System.IO.EndOfStreamException -> ()

    memory.Add program

let run () =
    let mutable finger = 0ul
    let mutable cycle = 0ul
    let mutable stop = false

    let mutable platter = 0ul
    let mutable code = 0uy
    let mutable a = 0uy
    let mutable b = 0uy
    let mutable c = 0uy
    let mutable value = 0ul

    while not stop && (int finger) < memory.[0].Count do
        // printfn "cycle: %u" cycle
        platter <- memory.[0].[int finger]
        code <- uint8 (platter >>> 28)

        match enum<Op>(int32 code) with
        | Op.ort ->
            a <- (uint8 (platter >>> 25)) &&& 7uy
            value <- platter &&& 0x1fffffful
            // printfn "%u(%u, %u)" code a value
            reg.[int a] <- value
        | _ ->
            a <- (uint8 (platter >>> 6)) &&& 7uy
            b <- (uint8 (platter >>> 3)) &&& 7uy
            c <- (uint8 platter) &&& 7uy

            // printfn "%u(%u, %u, %u)" code a b c
            // printfn "reg: %A" reg

            match enum<Op>(int32 code) with
            | Op.cmv -> if reg.[int c] <> 0ul then reg.[int a] <- reg.[int b]
            | Op.aix -> reg.[int a] <- memory.[int reg.[int b]].[int reg.[int c]]
            | Op.aam -> memory.[int reg.[int a]].[int reg.[int b]] <- reg.[int c]
            | Op.add -> reg.[int a] <- reg.[int b] + reg.[int c]
            | Op.mul -> reg.[int a] <- reg.[int b] * reg.[int c]
            | Op.dvi -> reg.[int a] <- reg.[int b] / reg.[int c]
            | Op.nad -> reg.[int a] <- ~~~(reg.[int b] &&& reg.[int c])
            | Op.out ->
                printf "%c" (char reg.[int c])
                stdout.Flush()
            | Op.lod ->
                if reg.[int b] <> 0ul then
                    memory.[0] <- new ResizeArray<uint32>(memory.[int reg.[int b]])
                finger <- reg.[int c] - 1ul
            | _ ->
                printfn "unknown code: %u" code
                stop <- true

        finger <- finger + 1ul
        cycle <- cycle + 1ul

load @"scrolls/sandmark.umz"
run()
