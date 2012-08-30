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
static volatile uint32_t posix_time __attribute__ ((section (".noinit")));
static volatile uint8_t posix_time_div __attribute__ ((section (".noinit")));
static volatile uint8_t posix_time_cksum __attribute__ ((section (".noinit")));

/* Private functions */
static uint8_t is_time_valid(void);
static uint8_t calc_posix_time_cksum(void);

/* Dividing 8 ms interval with 125 to get exactly 1 second */
#define POSIX_DIVIDER 125

/**
 * Increments millisecond counter.
 */
ISR(TIMER2_COMPA_vect)
{
	// Tick counter for effects
	ticks_volatile++;

	// Run real time clock if it is set
	if (is_time_valid()) {
		if (!--posix_time_div) {
			posix_time_div = POSIX_DIVIDER;
			posix_time++;
		}
		// Update checksum
		posix_time_cksum = calc_posix_time_cksum();		
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
		posix_time_div = POSIX_DIVIDER;
		posix_time_cksum = calc_posix_time_cksum();
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
		// Time is zero when it's not set
		time = is_time_valid() ? posix_time : 0;
 	}
	if (t != NULL) *t = time;
	return time;
}

/**
 * Calculates checksum using XOR and
 * seed value of 0x55.
 */
static uint8_t calc_posix_time_cksum(void) {
	return ((uint8_t)(posix_time >> 24) ^
		(uint8_t)(posix_time >> 16) ^
		(uint8_t)(posix_time >> 8) ^
		(uint8_t)posix_time_div ^
		(uint8_t)0x55);
}

/**
 * Validates checksum. May be called only when interrupts are
 * disabled.
 */
static uint8_t is_time_valid(void) {
	return posix_time_cksum == calc_posix_time_cksum();
}
