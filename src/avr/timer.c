#include <util/atomic.h>

/* ticks_volatile is incremented roughly every 1 millisecond and
 * overflows every 64th second. The tick counter is in effect_utils.c,
 * which should be updated by frame drawing code by calling
 * millis(). */
volatile uint16_t ticks_volatile = 0;

/**
 * Increments millisecond counter.
 */
ISR(TIMER2_COMPA_vect)
{
	ticks_volatile++;
}

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
