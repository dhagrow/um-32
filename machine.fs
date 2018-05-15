open System
open System.IO

type Op =
    | cmv=0 | aix=1 | aam=2 | add=3 | mul=4 | dvi=5 | nad=6 | hlt=7 | alc=8
    | abd=9 | out=10 | inp=11 | lod=12 | ort=13

let mutable memory = new ResizeArray<uint32 array>()
let mutable abandoned_indexes = new ResizeArray<uint32>()
let mutable reg: uint32 array = Array.zeroCreate 8

let load filename =
    let mutable program: uint32 array =
        Array.zeroCreate (int (FileInfo(filename).Length / 4L))
    let fp = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
    let reader = new BinaryReader(fp)
    let mutable index = 0
    try
        while true do
            let platter = reader.ReadInt32() |> System.Net.IPAddress.HostToNetworkOrder
            program.[index] <- uint32 platter
            index <- index + 1
    with
    | :? System.IO.EndOfStreamException -> ()

    memory.Add program

let state code a b c =
    printfn "%u(%u, %u, %u)" code a b c
    printfn "reg: %A" reg
    printfn "mem: %A" [for x in memory do yield x.Length]
    printfn "abd: %A" abandoned_indexes

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

    while not stop && (int finger) < memory.[0].Length do
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

            match enum<Op>(int32 code) with
            | Op.cmv -> if reg.[int c] <> 0ul then reg.[int a] <- reg.[int b]
            | Op.aix -> reg.[int a] <- memory.[int reg.[int b]].[int reg.[int c]]
            | Op.aam -> memory.[int reg.[int a]].[int reg.[int b]] <- reg.[int c]
            | Op.add -> reg.[int a] <- reg.[int b] + reg.[int c]
            | Op.mul -> reg.[int a] <- reg.[int b] * reg.[int c]
            | Op.dvi -> reg.[int a] <- reg.[int b] / reg.[int c]
            | Op.nad -> reg.[int a] <- ~~~(reg.[int b] &&& reg.[int c])
            | Op.hlt -> stop <- true
            | Op.alc ->
                let new_array = Array.zeroCreate (int reg.[int c])
                let index =
                    if abandoned_indexes.Count > 0 then
                        let i = abandoned_indexes.[0]
                        abandoned_indexes.RemoveAt(0)
                        memory.[int i] <- new_array
                        i
                    else
                        memory.Add new_array
                        uint32 memory.Count - 1ul
                reg.[int b] <- index
            | Op.abd ->
                memory.[int reg.[int c]] <- Array.empty
                abandoned_indexes.Add reg.[int c]
            | Op.out ->
                printf "%c" (char reg.[int c])
                stdout.Flush()
            | Op.inp -> reg.[int c] <- uint32 (Console.ReadKey()).KeyChar
            | Op.lod ->
                if reg.[int b] <> 0ul then
                    memory.[0] <- Array.copy memory.[int reg.[int b]]
                finger <- reg.[int c] - 1ul
            | _ ->
                printfn "unknown code: %u" code
                stop <- true

        finger <- finger + 1ul
        cycle <- cycle + 1ul


// allow ctrl-c to kill
System.Console.CancelKeyPress.Add(fun _ -> ())

load @"scrolls/codex.umz"
run()
