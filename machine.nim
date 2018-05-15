import endians
import streams
import strformat

var
  memory = newSeq[seq[uint32]]()

proc load(filename: string) =
  var fs = newFileStream(filename, fmRead)
  if isNil(fs):
    quit(QuitFailure)

  var program = newSeq[uint32]()
  var platter: uint32
  while not fs.atEnd:
    platter = fs.readUint32()
    bigEndian32(addr(platter), addr(platter))
    program.add(platter)
  fs.close()

  memory.setLen(0)
  memory.add(program)

proc run() =
  var
    finger = 0'u32
    cycle = 0'u32
    stop = false

    platter: uint32

  while not stop and finger < (uint32 memory[0].len):
    platter = memory[0][int finger]
    echo platter
    break

when isMainModule:
  load("scrolls/sandmark.umz")
  run()
