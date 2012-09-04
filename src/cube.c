
#include "cube.h"

// Define the buffers for LED cube grayscale data
uint8_t gs_buf_a[GS_BUF_BYTES]={0x00};
uint8_t gs_buf_b[GS_BUF_BYTES]={0x00};

uint8_t *gs_buf_front = gs_buf_a;
uint8_t *gs_buf_back = gs_buf_b;

/**
 * Swap buffers. Call this only from interrupt handlers or places
 * where no interrupts may occur.
 */
void gs_buf_swap(void) {
	uint8_t *tmp = gs_buf_front;
	gs_buf_front = gs_buf_back;
	gs_buf_back = tmp;
}

/**
 * Restore buffers after NO_FLIP effect. May be safely run even if the
 * last effect was FLIP effect. Must be called when there is no
 * possibility of gs_buf_swap happening the same time.
 */
void gs_restore_bufs(void) {
	if (gs_buf_front == gs_buf_a) {
		gs_buf_back = gs_buf_b;
	} else {
		gs_buf_back = gs_buf_a;
	}
}

// Some sanity checks
#if (LEDS_X * LEDS_Y * GS_DEPTH) > (8 * BYTES_PER_LAYER)
#error "There are more LED data on X-Y layer than there is BYTES_PER_LAYER"
#endif

#if (1 << (8*SHIFT_REGISTER_BYTES)) < LEDS_Z
#error "LEDS_Z is too large; does not fit inside SHIFT_REGISTER_BYTES"
#endif
