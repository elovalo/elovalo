#include <util/atomic.h>

/* ticks_volatile is incremented roughly every 1 millisecond and
 * overflows every 64th second. */
volatile uint16_t ticks_volatile = 0;

// TODO register timer and use it

uint16_t millis(void) {
	uint16_t copy_of_ticks;
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		copy_of_ticks = ticks_volatile;
	}
	return copy_of_ticks;
}

void reset_time(void) {
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		ticks_volatile = 0;
	}
}
