import os
import endians
import streams
import strformat

type
  Op = enum
    cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, otp, inp, lod, ort

var
  memory = newSeq[seq[uint32]]()
  abandoned_indexes = newSeq[uint32]()
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
      # echo &"{code}({a}, {value})"
      # echo &"reg: {reg}"

      reg[a] = value
    else:
      a = uint8((platter shr 6) and 7)
      b = uint8((platter shr 3) and 7)
      c = uint8(platter and 7)
      # echo &"{code}({a}, {b}, {c})"
      # echo &"reg: {reg}"

      case code
      of ord(cmv):
        if reg[c] != 0:
          reg[a] = reg[b]
      of ord(aix): reg[a] = memory[int reg[b]][int reg[c]]
      of ord(aam): memory[int reg[a]][int reg[b]] = reg[c]
      of ord(add): reg[a] = reg[b] + reg[c]
      of ord(mul): reg[a] = reg[b] * reg[c]
      of ord(dvi): reg[a] = reg[b] div reg[c]
      of ord(nad): reg[a] = not (reg[b] and reg[c])
      of ord(hlt): stop = true
      of ord(alc):
        var index = 0'u32
        let new_seq = newSeq[uint32](reg[c])
        if abandoned_indexes.len > 0:
          index = abandoned_indexes[0]
          abandoned_indexes.del(0)
          memory[int index] = new_seq
        else:
          index = uint32 memory.len
          memory.add(new_seq)
        reg[b] = index
      of ord(abd):
        memory[int reg[c]].setLen(0)
        abandoned_indexes.add(reg[c])
      of ord(otp):
        stdout.write(char reg[c])
        flushFile(stdout)
      of ord(inp):
        reg[c] = uint32 readChar(stdin)
      of ord(lod):
        if reg[b] != 0:
          memory[0] = memory[int reg[b]]
        finger = reg[c] - 1
      else:
        echo &"unknown code: {code}"
        break

    inc(finger)

when isMainModule:
  if paramCount() < 1:
    echo "usage: machine <source>"
  else:
    load(paramStr(1))
    run()
