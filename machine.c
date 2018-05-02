#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

uint32_t **memory;
uint32_t *array_sizes;

uint32_t *load(FILE *fp) {
    fseek(fp, 0L, SEEK_END);
    long size = ftell(fp);
    rewind(fp);

    uint32_t num_platters = size / sizeof(uint32_t);
    uint32_t *array = (uint32_t *) malloc(num_platters * sizeof(uint32_t));

    uint32_t *pos = array;
    size_t bytes_read = fread(pos, sizeof(*pos), 1, fp);
    while (bytes_read != 0) {
        ++pos;
        bytes_read = fread(pos, sizeof(*pos), 1, fp);
    }

    printf("pos: %li\n", pos - array);

    return array;
}

void cycle() {
    for (int i = 0; i < 10; i++) {
        printf("cycle: %d\n", i);
    }
}

int main(void) {
    // initialize array 0
    memory = (uint32_t **) malloc(sizeof(uint32_t));
    array_sizes = (uint32_t *) malloc(sizeof(uint32_t));

    FILE *fp = fopen("scrolls/sandmark.umz", "rb");
    memory[0] = load(fp);
    fclose(fp);

    cycle();

    free(memory[0]);
    free(array_sizes);
    free(memory);

    return 0;
}
