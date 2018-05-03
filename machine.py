#! /usr/bin/env python3

import struct
import itertools
from array import array as new_array

memory = {}
next_index = 0
abandoned_indexes = set()
reg = [0] * 8
finger = 0

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('program')
    parser.add_argument('-c', '--cycles', type=int,
        help='number of cycles to execute')
    args = parser.parse_args()

    with open(args.program, 'rb') as fp:
        memory[0] = load(fp)

    cycle(args.cycles)

def load(fp):
    array = new_array('I')
    buf = fp.read(4)
    while buf:
        platter = struct.unpack('>I', buf)[0]
        array.append(platter)
        buf = fp.read(4)
    return array

def cycle(limit=None):
    global finger
    i = 0
    code = None
    args = None

    try:
        for i in itertools.count():
            platter = memory[0][finger]
            code, args = unpack_op(platter)

            op = operators[code]
            op(*args)

            if limit is not None and i == limit:
                state('LIMIT', i, code, args)
                break

            finger += 1
    except Exception:
        state('FAIL', i, code, args)
        raise
    except:
        state('\nEXIT', i, code, args)
        raise

def unpack_op(platter):
    code = platter >> 28

    if code == 13:
        a = (platter >> 25) & 7
        value = platter & 0x1ffffff
        args = (a, value)
    else:
        a = (platter >> 6) & 7
        b = (platter >> 3) & 7
        c = platter & 7
        args = (a, b, c)

    return code, args

def state(msg, cycle=0, code=None, args=None):
    print('{} at {} (cycle {})'.format(msg, finger, cycle))
    if code is not None:
        name = operators[code].__name__
        print('{}|{}{}'.format(name, code, args))
    print('reg', reg)
    print('memory', {i: len(a) for i, a in itertools.islice(memory.items(), 10)})

##
## operators
##

def cmv(a, b, c):
    """
    #0. Conditional Move.

    The register A receives the value in register B, unless the register C
    contains 0.
    """
    if reg[c] != 0:
        reg[a] = reg[b]

def aix(a, b, c):
    """
    #1. Array Index.

    The register A receives the value stored at offset in register C in the
    array identified by B.
    """
    array = memory[reg[b]]
    reg[a] = array[reg[c]]

def aam(a, b, c):
    """
    #2. Array Amendment.

    The array identified by A is amended at the offset in register B to store
    the value in register C.
    """
    array = memory[reg[a]]
    array[reg[b]] = reg[c]

def add(a, b, c):
    """
    #3. Addition.

    The register A receives the value in register B plus the value in register
    C, modulo 2^32.
    """
    reg[a] = (reg[b] + reg[c]) % (2**32)

def mul(a, b, c):
    """
    #4. Multiplication.

    The register A receives the value in register B times the value in register
    C, modulo 2^32.
    """
    reg[a] = (reg[b] * reg[c]) % (2**32)

def div(a, b, c):
    """
    #5. Division.

    The register A receives the value in register B divided by the value in
    register C, if any, where each quantity is treated treated as an unsigned
    32 bit number.
    """
    reg[a] = reg[b] // reg[c]

def nad(a, b, c):
    """
    #6. Not-And.

    Each bit in the register A receives the 1 bit if either register B or
    register C has a 0 bit in that position.  Otherwise the bit in register A
    receives the 0 bit.
    """
    reg[a] = (reg[b] & reg[c]) ^ ((2**32) - 1)

def hlt(_a, _b, _c):
    """
    #7. Halt.

    The universal machine stops computation.
    """
    raise SystemExit

def alc(_a, b, c):
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
    memory[index] = new_array('I', [0] * reg[c])
    reg[b] = index

def abd(_a, _b, c):
    """
    #9. Abandonment.

    The array identified by the register C is abandoned. Future allocations
    may then reuse that identifier.
    """
    index = reg[c]
    del memory[index][:]
    abandoned_indexes.add(index)

def out(_a, _b, c):
    """
    #10. Output.

    The value in the register C is displayed on the console immediately. Only
    values between and including 0 and 255 are allowed.
    """
    print(chr(reg[c]), end='')

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

def lod(_a, b, c):
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
    global finger

    index = reg[b]
    if index != 0:
        memory[0] = memory[index][:]

    finger = reg[c] - 1

def ort(a, value):
    """
    #13. Orthography.

    The value indicated is loaded into the register A forthwith.
    """
    reg[a] = value

operators = [
    cmv, aix, aam, add, mul, div, nad, hlt, alc, abd, out, inp, lod, ort]

##
## entry
##

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        pass
