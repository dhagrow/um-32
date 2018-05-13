open System.IO

let mutable memory = new ResizeArray<ResizeArray<uint32>>()

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
    let finger = 0
    let stop = false

    printfn "len: %i" memory.Count

    while not stop do
        let platter = memory.[0].[finger]
        printfn "platter: %u" platter

load @"scrolls/sandmark.umz"
run()
