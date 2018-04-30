#! /usr/bin/env python

import struct
import ctypes
import itertools

import universal_machine

arrays = {}
next_index = 0
abandoned_indexes = set()
registers = [0] * 8
finger = [0, 0]

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('program')
    args = parser.parse_args()

    with open(args.program, 'rb') as fp:
        arrays[0] = load(fp)

    cycle()

def load(fp):
    array = []
    buf = fp.read(4)
    while buf:
        platter = struct.unpack('>I', buf)[0]
        array.append(platter)
        buf = fp.read(4)
    return array

def cycle():
    um = universal_machine.um()
    with open('sandmark.umz', 'rb') as fp:
        um.mem.append(load(fp))

    for i in itertools.count():
        platter = arrays[finger[0]][finger[1]]
        op = Operator(platter)

        try:
            op.operate()
        except Exception:
            print('FAIL on {}:{}'.format(*finger))
            print(op)
            print('registers', registers)
            print('arrays', {i: len(a) for i, a in arrays.items()})
            raise

        finger[1] += 1

        um.step()
        if um.registers != registers:
            import code
            l = locals()
            l.update(globals())
            code.interact(local=l)

##
## structs
##

class OperatorBits(ctypes.LittleEndianStructure):
    _fields_ = [
        ('c',    ctypes.c_uint32, 3),
        ('b',    ctypes.c_uint32, 3),
        ('a',    ctypes.c_uint32, 3),
        ('null', ctypes.c_uint32, 19),
        ('code', ctypes.c_uint32, 4),
        ]

    def __str__(self):
        return '{} {} {} {}'.format(self.code, self.a, self.b, self.c)

class OrtOperatorBits(ctypes.LittleEndianStructure):
    _fields_ = [
        ('value', ctypes.c_uint32, 25),
        ('a',     ctypes.c_uint32, 3),
        ('code',  ctypes.c_uint32, 4),
        ]

    def __str__(self):
        return '{} {} {}'.format(self.code, self.a, self.value)

class Operator(ctypes.Union):
    _fields_ = [
        ('b', OperatorBits),
        ('o', OrtOperatorBits),
        ('platter', ctypes.c_uint32),
        ]

    def __init__(self, platter):
        super().__init__()
        self.platter = platter

    @property
    def name(self):
        return operators[self.b.code].__name__

    def operate(self):
        func = operators[self.b.code]
        func(self)

    def __str__(self):
        d = self.o if self.name == 'ort' else self.b
        return '{}[{}]'.format(self.name, d)

##
## operators
##

def cmv(op):
    """
    #0. Conditional Move.

    The register A receives the value in register B, unless the register C
    contains 0.
    """
    if registers[op.b.c] != 0:
        registers[op.b.a] = registers[op.b.b]

def aix(op):
    """
    #1. Array Index.

    The register A receives the value stored at offset in register C in the
    array identified by B.
    """
    array = arrays[registers[op.b.b]]
    registers[op.b.a] = array[registers[op.b.c]]

def aam(op):
    """
    #2. Array Amendment.

    The array identified by A is amended at the offset in register B to store
    the value in register C.
    """
    array = arrays[registers[op.b.a]]
    array[registers[op.b.b]] = registers[op.b.c]

def add(op):
    """
    #3. Addition.

    The register A receives the value in register B plus the value in register
    C, modulo 2^32.
    """
    registers[op.b.a] = (registers[op.b.b] + registers[op.b.c]) % (2**32)

def mul(op):
    """
    #4. Multiplication.

    The register A receives the value in register B times the value in register
    C, modulo 2^32.
    """
    registers[op.b.a] = (registers[op.b.b] * registers[op.b.c]) % (2**32)

def div(op):
    """
    #5. Division.

    The register A receives the value in register B divided by the value in
    register C, if any, where each quantity is treated treated as an unsigned
    32 bit number.
    """
    registers[op.b.a] = registers[op.b.a] // registers[op.b.c]

def nad(op):
    """
    #6. Not-And.

    Each bit in the register A receives the 1 bit if either register B or
    register C has a 0 bit in that position.  Otherwise the bit in register A
    receives the 0 bit.
    """
    registers[op.b.a] = (registers[op.b.b] & registers[op.b.c]) ^ ((2**32) - 1)

def hlt(_):
    """
    #7. Halt.

    The universal machine stops computation.
    """
    raise SystemExit

def alc(op):
    """
    #8. Allocation.

    A new array is created with a capacity of platters commensurate to the
    value in the register C. This new array is initialized entirely with
    platters holding the value 0. A bit pattern not consisting of exclusively
    the 0 bit, and that identifies no other active allocated array, is placed
    in the B register.
    """
    global next_index
    try:
        index = abandoned_indexes.pop()
    except KeyError:
        next_index += 1
        index = next_index
    arrays[index] = [0] * registers[op.b.c]
    registers[op.b.b] = index

def abd(op):
    """
    #9. Abandonment.

    The array identified by the register C is abandoned. Future allocations
    may then reuse that identifier.
    """
    del arrays[registers[op.b.c]][:]
    abandoned_indexes.add(registers[op.b.c])

def out(op):
    """
    #10. Output.

    The value in the register C is displayed on the console immediately. Only
    values between and including 0 and 255 are allowed.
    """
    print(chr(registers[op.b.c]), end='')

def inp():
    """
    #11. Input.

    The universal machine waits for input on the console. When input arrives,
    the register C is loaded with the input, which must be between and
    including 0 and 255. If the end of input has been signaled, then the
    register C is endowed with a uniform value pattern where every place is
    pregnant with the 1 bit.
    """
    pass

def lod(op):
    """
    #12. Load Program.

    The array identified by the B register is duplicated and the duplicate
    shall replace the '0' array, regardless of size. The execution finger is
    placed to indicate the platter of this array that is described by the
    offset given in C, where the value 0 denotes the first platter, 1 the
    second, et cetera.

    The '0' array shall be the most sublime choice for loading, and shall be
    handled with the utmost velocity.
    """
    array = arrays[registers[op.b.b]]
    arrays[0] = array[:]

    finger[0] = 0
    finger[1] = registers[op.b.c] - 1

def ort(op):
    """
    #13. Orthography.

    The value indicated is loaded into the register A forthwith.
    """
    registers[op.o.a] = op.o.value

operators = [
    cmv, aix, aam, add, mul, div, nad, hlt, alc, abd, out, inp, lod, ort]

##
## entry
##

if __name__ == '__main__':
    main()
