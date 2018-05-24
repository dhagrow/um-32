import std.stdio, std.stdint;

void main() {
    File f = File("scrolls/sandmark.umz");
    auto buf = f.rawRead(new ubyte[4]);
    auto platter = *cast(uint32_t*) buf;
    platter.writeln;
}
