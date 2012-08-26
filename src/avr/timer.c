#include <util/atomic.h>
#include <stdlib.h>
#include "timer.h"

/* ticks_volatile is incremented roughly every 1 millisecond and
 * overflows every 64th second. The tick counter is in effect_utils.c,
 * which is copied from this variable by centisecs(). */
static volatile uint16_t ticks_volatile = 0;

/* Real time clock is 32 bit second counter starting from 1970-01-01
 * 00:00:00 +0000 (UTC). This counter is unsigned, so expect it to
 * work until 2016. */
static volatile uint32_t posix_time = 0;

/**
 * Increments millisecond counter.
 */
ISR(TIMER2_COMPA_vect)
{
	// Tick counter for effects
	ticks_volatile++;

	/* Count seconds by dividing 8 ms interval with 125 (refill
	   value is 126 because of pre-decrement) */
	static uint8_t div = 126;
	if (posix_time && !--div) {
		// Exactly one second has passed
		div = 126;
		posix_time++;
	}
}

/**
 * Returns the time in centiseconds (not really, 0.008 seconds after
 * clock granularity change). TODO rename this function. */
uint16_t centisecs(void) {
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

/**
 * Sets POSIX time to this device. Always succeedes and returns 0.
 */
int stime(time_t *t) {
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		posix_time = *t;
	}
	return 0;
}

/**
 * Returns POSIX time. If the clock is not running (never set by
 * stime) it returns 0. (this part doesn't conform POSIX)
 */
time_t time(time_t *t) {
	uint32_t time;
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		time = posix_time;
 	}
	if (t != NULL) *t = time;
	return time;
}
