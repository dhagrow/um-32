#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <byteswap.h>

uint32_t **memory;
uint32_t *array_sizes;
uint32_t finger;

void cycle() {
    printf("platters: %u\n", array_sizes[0]);

    uint32_t platter;
    finger = 0;

    while (finger < array_sizes[0]) {
        platter = memory[0][finger];
        printf("platter: %u\n", __bswap_32(platter));
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
    size_t bytes_read = fread(pos, sizeof(*pos), 1, fp);
    while (bytes_read != 0) {
        ++pos;
        bytes_read = fread(pos, sizeof(*pos), 1, fp);
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
