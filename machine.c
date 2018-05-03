#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <byteswap.h>

uint8_t volatile stop = 0;
uint32_t **memory;
uint32_t *array_sizes;
uint32_t cycle;
uint32_t finger;
uint32_t reg[8] = {0, 0, 0, 0, 0, 0, 0, 0};

enum Operators {
    cmv, aix, aam, add, mul, divi, nad, hlt, alc, abd, out, inp, lod, ort};

void state(uint8_t code, uint8_t a, uint8_t b, uint8_t c) {
    printf("\nSTOP at %u (cycle %u)\n", finger, cycle);
    printf("op|%u(%u, %u, %u)\n", code, a, b, c);
    printf("reg [");
    for (int i=0; i < 7; i++) {
        printf("%u, ", reg[i]);
    }
    printf("%u]\n", reg[7]);
    printf("memory {0: %u}\n", array_sizes[0]);
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

    while (!stop && finger < array_sizes[0]) {
        platter = memory[0][finger];
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
        case add: // 03
            reg[a] = (reg[b] + reg[c]) % (1UL << 32);
            break;
        case mul: // 04
            reg[a] = (reg[b] * reg[c]) % (1UL << 32);
            break;
        case nad: // 06
            reg[a] = (reg[b] & reg[c]) ^ ((1UL << 32) - 1);
            break;
        case out: // 10
            putchar(reg[c]);
            break;
        case lod: // 12
            if (reg[b] != 0) {
                free(memory[0]);
                val = array_sizes[reg[b]]; // size
                memory[0] = (uint32_t *) malloc(val * sizeof(uint32_t));
                memcpy(memory[0], memory[reg[b]], val * sizeof(uint32_t));
                array_sizes[0] = val;
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

uint32_t load(FILE *fp, uint32_t **array) {
    fseek(fp, 0L, SEEK_END);
    long size = ftell(fp);
    rewind(fp);

    uint32_t num_platters = size / sizeof(uint32_t);
    *array = (uint32_t *) malloc(num_platters * sizeof(uint32_t));

    uint32_t *pos = *array;
    size_t bytes_read;
    while (1) {
        bytes_read = fread(pos, sizeof(*pos), 1, fp);
        if (bytes_read != 1) break;
        *pos = __bswap_32(*pos);
        ++pos;
    }

    return num_platters;
}

void sig_handler(int sig) {
    stop = 1;
}

int main(int argc, char *argv[]) {
    signal(SIGINT, sig_handler);

    uint32_t limit = argc == 2 ? atoi(argv[1]) : 0;

    // initialize array 0
    memory = (uint32_t **) malloc(sizeof(uint32_t));
    array_sizes = (uint32_t *) malloc(sizeof(uint32_t));

    FILE *fp = fopen("scrolls/sandmark.umz", "rb");
    array_sizes[0] = load(fp, &(memory[0]));
    fclose(fp);

    run(limit);

    free(memory[0]);
    free(array_sizes);
    free(memory);

    return 0;
}
