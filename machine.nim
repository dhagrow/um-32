import endians
import streams
import strformat

type
  Op = enum
    cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, otp, inp, lod, ort

var
  memory = newSeq[seq[uint32]]()
  reg: array[0..7, uint32]

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
    code: uint8
    a: uint8
    b: uint8
    c: uint8
    value: uint32

  while not stop and finger < (uint32 memory[0].len):
    platter = memory[0][int finger]
    code = uint8(platter shr 28)

    case code
    of ord(ort):
      a = uint8((platter shr 25) and 7)
      value = platter and 0x1ffffff
      echo &"{code}({a}, {value})"
      echo &"reg: {reg}"

      reg[a] = value
    else:
      a = uint8((platter shr 6) and 7)
      b = uint8((platter shr 3) and 7)
      c = uint8(platter and 7)
      echo &"{code}({a}, {b}, {c})"
      echo &"reg: {reg}"

      case code
      of ord(cmv):
        if reg[c] != 0:
          reg[a] = reg[b]
      of ord(add):
        reg[a] = reg[b] + reg[c]
      else:
        echo &"unknown code: {code}"
        break

    inc(finger)

when isMainModule:
  load("scrolls/sandmark.umz")
  run()
