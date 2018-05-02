#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

uint32_t **memory;

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

    printf("pos: %d", pos - array);

    return array;
}

int main(void) {
    FILE *fp = fopen("scrolls/sandmark.umz", "rb");
    uint32_t *array = load(fp);
    fclose(fp);

    free(array);

    return 0;
}
