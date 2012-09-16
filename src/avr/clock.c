/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <util/atomic.h>
#include <stdlib.h>
#include "clock.h"
#include "cron.h"

/* ticks_volatile is incremented roughly every 1 millisecond and
 * overflows every 64th second. The tick counter is in effect_utils.c,
 * which is copied from this variable by centisecs(). */
static volatile uint16_t ticks_volatile = 0;

/* Real time clock is 32 bit second counter starting from 1970-01-01
 * 00:00:00 +0000 (UTC). This counter is unsigned, so expect it to
 * work until 2016. */
static volatile struct {
	uint32_t time;
	uint8_t div;
	uint8_t cksum;
} rtc __attribute__ ((section (".noinit")));

/* Private functions */
static uint8_t is_time_valid(void);
static uint8_t calc_posix_time_cksum(void);
static void enable_interrupts_and_run_cron(void);

/* Dividing 8 ms interval with 125 to get exactly 1 second */
#define POSIX_DIVIDER 125

/**
 * Timer interrupt increments RTC and tick counter
 */
ISR(TIMER2_COMPA_vect)
{
	/* Tick counter for effects. Stop incrementing after maximum
	 * is reached to avoid messing the effect */
	if (ticks_volatile != ~0)
		ticks_volatile++;

	// Run real time clock if it is set
	if (!--rtc.div && is_time_valid()) {
		rtc.div = POSIX_DIVIDER;
		rtc.time++;

		// Update checksum
		rtc.cksum = calc_posix_time_cksum();

		/* ATTENTION! Running of cron enables interrupts. This
		 * must be last line of interrupt handler! */
		enable_interrupts_and_run_cron();
	}
}

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

time_t time(time_t *t) {
	time_t time;
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		// Time is zero when it's not set
		time = is_time_valid() ? rtc.time : 0;
 	}
	if (t != NULL) *t = time;
	return time;
}

int stime(time_t *t) {
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		rtc.time = *t;
		rtc.div = POSIX_DIVIDER;
		rtc.cksum = calc_posix_time_cksum();
	}
	return 0;
}

time_t unsafe_time(time_t *t) {
	if (t != NULL) *t = rtc.time;
	return rtc.time;
}

/**
 * Calculates checksum using XOR and seed value of 0x55. Must be
 * called with interrupts disabled.
 */
static uint8_t calc_posix_time_cksum(void) {
	uint8_t *p = (void*)&(rtc.time);
	return 0x55 ^ p[0] ^ p[1] ^ p[2] ^ p[3];
}

/**
 * Validates checksum. May be called only when interrupts are
 * disabled.
 */
static uint8_t is_time_valid(void) {
	return calc_posix_time_cksum() == rtc.cksum;
}

/**
 * Runs cron with enabled interrupts. Detects if a cron run has been
 * running too long.
 */
void enable_interrupts_and_run_cron(void) {
	static uint8_t cron_running = 0;

	if (cron_running) {
		// TODO light up debug LED
		return;
	}
	
	cron_running = 1;
	const time_t now = rtc.time; // Storing over sei()
	sei();
	run_cron(now);
	cron_running = 0;
}
