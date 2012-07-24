#ifndef CUBE_H_
#define CUBE_H_

#include <stdint.h>
#include "env.h"

// Total data in a buffer
#define GS_BUF_BYTES (LEDS_Z * BYTES_PER_LAYER)

/* Front and back buffers. Front is the one being drawn on and back is
 * the one that should be manipulated by effects. There are some
 * exceptions to this rule when doing some very nasty effects. */
extern uint8_t *FrontBuffer;
extern uint8_t *BackBuffer;

#endif /* CUBE_H_ */
