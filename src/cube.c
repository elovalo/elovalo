
#include "cube.h"

// Define the buffers for LED cube grayscale data
uint8_t gs_buf_a[GS_BUF_BYTES]={0x00};
uint8_t gs_buf_b[GS_BUF_BYTES]={0x00};

uint8_t *FrontBuffer = gs_buf_a;
uint8_t *BackBuffer = gs_buf_b;

// Some sanity checks
#if (LEDS_X * LEDS_Y * GS_DEPTH) > (8 * BYTES_PER_LAYER)
#error "There are more LED data on X-Y layer than there is BYTES_PER_LAYER"
#endif

#if (1 << (8*SHIFT_REGISTER_BYTES)) < LEDS_Z
#error "LEDS_Z is too large; does not fit inside SHIFT_REGISTER_BYTES"
#endif
