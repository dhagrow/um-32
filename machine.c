#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <byteswap.h>

uint8_t volatile stop = 0;
uint32_t **memory;
uint32_t *array_sizes;
uint32_t finger;
uint32_t reg[8] = {0, 0, 0, 0, 0, 0, 0, 0};

enum Operators {
    cmv, aix, aam, add, mul, divi, nad, hlt, alc, abd, out, inp, lod, ort};

void state() {
    for (int i=0; i < 8; i++) {
        printf("%d=%u ", i, reg[i]);
    }
    printf("\n");
}

void cycle() {
    uint32_t platter;
    uint8_t code;
    uint8_t a;
    uint8_t b;
    uint8_t c;
    uint32_t tmp; // misc values

    finger = 0;

    while (!stop && finger < array_sizes[0]) {
        platter = memory[0][finger];
        code = (platter >> 28);

        if (code == ort) {
            a = (platter >> 25) & 7;
            b = platter & 0x1ffffff;
        } else {
            a = (platter >> 6) & 7;
            b = (platter >> 3) & 7;
            c = platter & 7;
        }

        switch (code) {
        case cmv: // 00
            if (reg[c] != 0)
                reg[a] = reg[b];
            break;
        case add: // 03
            reg[a] = (reg[b] + reg[c]) % (1UL << 32);
            break;
        case mul: // 04
            reg[a] = (reg[b] * reg[c]) % (1UL << 32);
            break;
        case nad: // 06
            reg[a] = !(reg[b] & reg[c]);
            break;
        case out: // 10
            putchar(reg[c]);
            break;
        case lod: // 12
            if (reg[b] != 0) {
                free(memory[0]);
                tmp = array_sizes[reg[b]]; // size
                memory[0] = (uint32_t *) malloc(tmp * sizeof(uint32_t));
                memcpy(memory[0], memory[reg[b]], tmp * sizeof(uint32_t));
                array_sizes[0] = tmp;
            }
            finger = reg[c] - 1;
            break;
        case ort: // 13
            reg[a] = b;
            break;
        default:
            printf("\nunknown code: %u\n", code);
            state();
            return;
        };

        finger++;
    }

    if (stop) {
        printf("\nop: %u a=%u b=%u c=%u pc=%u\n", code, a, b, c, finger);
        state();
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

int main(void) {
    signal(SIGINT, sig_handler);

    // initialize array 0
    memory = (uint32_t **) malloc(sizeof(uint32_t));
    array_sizes = (uint32_t *) malloc(sizeof(uint32_t));

    FILE *fp = fopen("scrolls/sandmark.umz", "rb");
    array_sizes[0] = load(fp, &(memory[0]));
    fclose(fp);

    cycle();

    free(memory[0]);
    free(array_sizes);
    free(memory);

    return 0;
}
