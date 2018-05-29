import std.conv, std.stdio, std.stdint, std.bitmanip, std.format, std.container;

auto memory = new uint32_t[][](0, 0);
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
                case Op.cmv:
                    if (reg[c] != 0) {
                        reg[a] = reg[b];
                    }
                    break;
                case Op.add:
                    reg[a] = (reg[b] + reg[c]) % (1UL << 32);
                    break;
                case Op.lod:
                    if (reg[b] != 0) {
                        memory[0] = memory[reg[b]];
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
    memory.length = 0;
    memory.length = 1;

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
