#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <byteswap.h>

typedef struct {
    uint32_t *data;
    uint32_t size;
} array;

uint8_t volatile stop = 0;
array *memory;
uint32_t mem_size;
uint32_t cycle;
uint32_t finger;
uint32_t reg[8] = {0, 0, 0, 0, 0, 0, 0, 0};

enum Operators {
    cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, out, inp, lod, ort};

uint32_t new_array(uint32_t size) {
    uint32_t index;
    array *tmp = realloc(memory, (mem_size + 1) * sizeof(*tmp));

    if (!tmp) {
        // fail
    } else {
        index = mem_size;
        mem_size++;
        memory = tmp;

        memory[index].data = calloc(size, sizeof(uint32_t));
        memory[index].size = size;
    }

    return index;
}

uint32_t free_array(uint32_t index) {
    free(memory[index].data);
    memory[index].data = 0;
}

void state(uint8_t code, uint8_t a, uint8_t b, uint8_t c) {
    printf("\nSTOP at %u (cycle %u)\n", finger, cycle);
    printf("op|%u(%u, %u, %u)\n", code, a, b, c);
    printf("reg [");
    for (int i=0; i < 7; i++) {
        printf("%u, ", reg[i]);
    }
    printf("%u]\n", reg[7]);
    printf("memory {0: %u}\n", memory[0].size);
}

void run(uint32_t limit) {
    uint32_t platter;
    uint8_t code;
    uint8_t a;
    uint8_t b;
    uint8_t c;
    uint32_t val;

    cycle = 0;
    finger = 0;

    while (!stop && finger < memory[0].size) {
        platter = memory[0].data[finger];
        code = (platter >> 28);

        if (code == ort) {
            a = (platter >> 25) & 7;
            val = platter & 0x1ffffff;
        } else {
            a = (platter >> 6) & 7;
            b = (platter >> 3) & 7;
            c = platter & 7;
        }

        switch (code) {
        case cmv: // 00
            if (reg[c] != 0) {
                reg[a] = reg[b];
            }
            break;
        case aix: // 01
            reg[a] = memory[reg[b]].data[reg[c]];
            break;
        case aam: // 02
            memory[reg[a]].data[reg[b]] = reg[c];
            break;
        case add: // 03
            reg[a] = (reg[b] + reg[c]) % (1UL << 32);
            break;
        case mul: // 04
            reg[a] = (reg[b] * reg[c]) % (1UL << 32);
            break;
        case dvi: // 05
            reg[a] = reg[b] / reg[c];
            break;
        case nad: // 06
            reg[a] = (reg[b] & reg[c]) ^ ((1UL << 32) - 1);
            break;
        case alc: // 08
            reg[b] = new_array(reg[c]);
            break;
        case abd: // 09
            free_array(reg[c]);
            break;
        case out: // 10
            putchar(reg[c]);
            break;
        case inp: // 11
            reg[c] = getchar();
            break;
        case lod: // 12
            if (reg[b] != 0) {
                free(memory[0].data);
                val = memory[reg[b]].size;
                memory[0].data = malloc(val * sizeof(uint32_t));
                memory[0].size = val;
                memcpy(memory[0].data, memory[reg[b]].data,
                    val * sizeof(uint32_t));
            }
            finger = reg[c] - 1;
            break;
        case ort: // 13
            reg[a] = val;
            break;
        default:
            printf("unknown code: %u\n", code);
            state(code, a, b, c);
            return;
        };

        if (limit > 0 && cycle == limit) {
            stop = 1;
            break;
        }

        finger++;
        cycle++;
    }

    if (stop) {
        state(code, a, b, c);
    }
}

void load(FILE *fp, array *arr) {
    fseek(fp, 0L, SEEK_END);
    long size = ftell(fp);
    rewind(fp);

    arr->data = malloc(size);
    arr->size = size / sizeof(uint32_t);

    uint32_t *pos = arr->data;
    size_t bytes_read;
    while (1) {
        bytes_read = fread(pos, sizeof(*pos), 1, fp);
        if (bytes_read != 1) break;
        *pos = __bswap_32(*pos);
        ++pos;
    }
}

void sig_handler(int sig) {
    stop = 1;
}

int main(int argc, char *argv[]) {
    signal(SIGINT, sig_handler);

    if (argc < 2 || argc > 3) {
        printf("usage: machine <source> [cycle-limit]\n");
        return 0;
    }

    char *path = argv[1];
    uint32_t limit = argc == 3 ? atoi(argv[2]) : 0;

    // initialize array 0
    memory = malloc(sizeof(*memory));
    mem_size = 1;

    FILE *fp = fopen(path, "rb");
    load(fp, &(memory[0]));
    fclose(fp);

    run(limit);

    free(memory[0].data);
    free(memory);

    return 0;
}
