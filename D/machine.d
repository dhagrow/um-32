import std.conv, std.array, std.stdio, std.stdint, std.bitmanip, std.format,
    std.container, std.algorithm.mutation, std.range.primitives;

auto memory = new uint32_t[][](0, 0);
auto abandoned_indexes = new uint32_t[](0);
uint32_t[8] reg;

enum Op {cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, otp, inp, lod, ort};

void run() {
    bool stop = false;
    auto cycle = 0;
    uint32_t finger = 0;
    uint32_t platter = 0;
    Op op;
    uint8_t code = 0;
    uint8_t a = 0;
    uint8_t b = 0;
    uint8_t c = 0;
    uint32_t val = 0;

    while (!stop) {
        platter = memory[0][finger];
        code = (platter >> 28);
        op = to!Op(code);

        // writefln("cycle: %s finger: %s reg: %s", cycle, finger, reg);

        if (op == Op.ort) {
            a = (platter >> 25) & 7;
            val = platter & 0x1ffffff;
            // writefln("%s(%s, %s)", op, a, val);

            reg[a] = val;
        } else {
            a = (platter >> 6) & 7;
            b = (platter >> 3) & 7;
            c = platter & 7;
            // writefln("%s(%s, %s, %s)", op, a, b, c);

            switch (op) {
                case Op.cmv: // 00
                    if (reg[c] != 0) {
                        reg[a] = reg[b];
                    }
                    break;
                case Op.aix: // 01
                    reg[a] = memory[reg[b]][reg[c]];
                    break;
                case Op.aam: // 02
                    memory[reg[a]][reg[b]] = reg[c];
                    break;
                case Op.add: // 03
                    reg[a] = (reg[b] + reg[c]) % (1UL << 32);
                    break;
                case Op.mul: // 04
                    reg[a] = (reg[b] * reg[c]) % (1UL << 32);
                    break;
                case Op.dvi: // 05
                    reg[a] = reg[b] / reg[c];
                    break;
                case Op.nad: // 06
                    reg[a] = (reg[b] & reg[c]) ^ ((1UL << 32) - 1);
                    break;
                case Op.alc: // 08
                    uint32_t index;
                    auto new_array = new uint32_t[](reg[c]);
                    fill(new_array, 0);
                    if (abandoned_indexes.length != 0) {
                        index = abandoned_indexes.back;
                        abandoned_indexes.popBack();
                        memory[index] = new_array;
                    } else {
                        memory ~= new_array;
                        index = to!uint32_t(memory.length) - 1;
                    }
                    reg[b] = index;
                    break;
                case Op.abd: // 09
                    memory[reg[c]].length = 0;
                    abandoned_indexes.length++;
                    abandoned_indexes[abandoned_indexes.length - 1] = reg[c];
                    break;
                case Op.otp: // 10
                    putchar(reg[c]);
                    stdout.flush();
                    break;
                case Op.lod: // 12
                    if (reg[b] != 0) {
                        memory[0] = array(memory[reg[b]]);
                    }
                    finger = reg[c] - 1;
                    break;
                default:
                    throw new Exception(format("unknown op: %s", op));
            }
        }

        ++finger;
        ++cycle;
    }
}

void load() {
    memory.assumeSafeAppend();
    memory.length = 0;
    memory.length = 1;
    memory[0].assumeSafeAppend();

    File f = File("scrolls/sandmark.umz");
    while (!f.eof()) {
        auto chunk = *cast(uint32_t*) f.rawRead(new ubyte[4]);
        auto platter = nativeToBigEndian(chunk);
        memory[0] ~= *cast(uint32_t*) platter;
    }
}

void main() {
    load();
    run();
}
