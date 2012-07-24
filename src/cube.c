
#include "cube.h"
#include "env.h"

// Round the number of bytes per layer towards next full byte
#define BYTES_IN_LAYER ((LEDS_X*LEDS_Y*GS_DEPTH+7) >> 3)
#define BYTES_IN_CUBE (LEDS_Z * BYTES_IN_LAYER)

// Define the buffers.
uint8_t gs_buf_a[BYTES_IN_CUBE]={0x00};
uint8_t gs_buf_b[BYTES_IN_CUBE]={0x00};

//Pointer to the GS data buffer that holds the data to be sent to the TLC5940
uint8_t *FrontBuffer = gs_buf_a;
uint8_t *BackBuffer = gs_buf_b;
