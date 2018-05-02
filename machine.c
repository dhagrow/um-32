#include <stdio.h>
#include <stdint.h>

uint32_t *memory;

int main(void) {
    size_t bytes_read;
    uint32_t platter;

    FILE *fp = fopen("scrolls/sandmark.umz", "rb");
    bytes_read = fread(&platter, sizeof platter, 1, fp);

    printf("read: %d platter: %u", bytes_read, platter);

    fclose(fp);

    return 0;
}
