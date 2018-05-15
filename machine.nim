import endians
import streams
import strformat

var fs = newFileStream("scrolls/sandmark.umz", fmRead)
if not isNil(fs):
  var platter: uint32
  while not fs.atEnd:
    platter = fs.readUint32()
    bigEndian32(addr(platter), addr(platter))
    echo &"{platter:032b}\n"
  fs.close()
