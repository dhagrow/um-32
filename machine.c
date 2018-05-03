#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <byteswap.h>

uint32_t **memory;
uint32_t *array_sizes;
uint32_t finger;
uint32_t reg[8] = {0, 0, 0, 0, 0, 0, 0, 0};

enum Operators {
    cmv, aix, aam, add, mul, divi, nad, hlt, alc, abd, out, inp, lod, ort};

union Operator {
    struct {
        uint32_t c: 3;
        uint32_t b: 3;
        uint32_t a: 3;
        uint32_t _: 19;
        enum Operators code : 4;
    } b;
    uint32_t value;
};

union Orthography {
    struct {
        uint32_t value: 25;
        uint32_t a: 3;
        enum Operators code : 4;
    } b;
    uint32_t value;
};

void state() {
    for (int i=0; i < 8; i++) {
        printf("%d: %u\n", i, reg[i]);
    }
}

void cycle() {
    printf("platters: %u\n", array_sizes[0]);

    union Operator op;
    finger = 0;

    while (finger < array_sizes[0]) {
        op.value = memory[0][finger];

        printf("code: %u\n", op.b.code);

        switch (op.b.code) {
        case cmv:
            if (reg[op.b.c] != 0)
                reg[op.b.a] = reg[op.b.b];
            break;
        case add:
            reg[op.b.a] = (reg[op.b.b] + reg[op.b.c]) % (1UL << 32);
            break;
        default:
            printf("unknown code: %u\n", op.b.code);
        };

        state();

        finger++;
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

int main(void) {
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
