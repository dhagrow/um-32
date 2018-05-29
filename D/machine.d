import std.conv, std.stdio, std.stdint, std.format, std.container;

auto memory = new uint32_t[][](0, 0);

enum Op {cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, otp, inp, lod, ort};

void run() {
    bool stop = false;
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

        if (op == Op.ort) {
            a = (platter >> 25) & 7;
            val = platter & 0x1ffffff;
            writefln("%s(%s, %s)", op, a, val);
        } else {
            a = (platter >> 6) & 7;
            b = (platter >> 3) & 7;
            c = platter & 7;
            writefln("%s(%s, %s, %s)", op, a, b, c);

            switch (op) {
                default:
                    throw new Exception(format("unknown op: %s", op));
            }
        }

        ++finger;
    }
}

void load() {
    memory.length = 0;
    memory.length = 1;

    File f = File("scrolls/sandmark.umz");
    while (!f.eof()) {
        auto platter = *cast(uint32_t*) f.rawRead(new ubyte[4]);
        memory[0] ~= platter;
    }
}

void main() {
    load();
    run();
}
