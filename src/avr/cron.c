/**
 * Functions for accessing schedule
 */

#include <stdbool.h>
#include <stdlib.h>
#include "../pgmspace.h"
#include "cron.h"
#include "powersave.h"
#include "configuration.h"
#include "serial.h" // For temporary debug code

#define SECS_IN_DAY ((time_t)60*60*24)

// The info table for serial port access

PROGMEM const char s_cube_shutdown[] = "Power-off the cube part";
PROGMEM const char s_cube_start[] = "Power-on the cube part";
PROGMEM const char s_serial_hello[] = "Say \"hello\" to serial console";
PROGMEM const char s_serial_hello_arg[] = "Number to write after \"hello\"";

const struct action_info cron_actions[] PROGMEM = { 
	{ &cube_shutdown, s_cube_shutdown, NULL },
	{ &cube_start, s_cube_start, NULL },
	{ &serial_hello, s_serial_hello, s_serial_hello_arg}
};

#define CRON_ACTIONS_LEN (sizeof(cron_actions)/sizeof(struct action_info))

/**
 * Validate event contents (before accepting it from serial console).
 */
bool validate_event(struct event *e)
{
	// Validate kind. END is not there for a reason.
	if (e->kind != WEEKLY &&
	    e->kind != ONETIME &&
	    e->kind != EMPTY) return false;

	// Validate act
	for (uint8_t i=0; i<CRON_ACTIONS_LEN; i++) {
		action_t x = (action_t)pgm_get(cron_actions[i].act, word);
		if (e->act == x) return true;
	}
	return false;
}

/**
 * Checks if it any actions are needed to be run. May be run at
 * arbitary intervals, though running it once per minute is a wise
 * choice. NB. Interrupts should be enabled when calling this!
 */ 
void run_cron(const time_t now) {
	static time_t last_time = 0;

	time_t start_of_day = now / SECS_IN_DAY * SECS_IN_DAY;
	int16_t last_min = (last_time < start_of_day)
		? -1
		: (last_time-start_of_day)/60;
	int16_t now_min = (now - start_of_day)/60;
	uint8_t weekday = (uint8_t)(now / SECS_IN_DAY % 7);

	for (uint8_t i=0; i<CRONTAB_SIZE; i++) {
		struct event e;
		get_crontab_entry(&e,i);
		
		// Doing the checks
		if (e.kind == END) break;
		if (e.kind == EMPTY) continue;
		if (e.kind == WEEKLY) {
			// If not this day, skip
			if (!(e.u.weekly.weekdays & (1 << weekday))) continue;
			// If haven't happened, skip
			if (e.u.weekly.minutes <= last_min) continue;
			if (e.u.weekly.minutes > now_min) continue;
		} else if (e.kind == ONETIME) {
			if (e.u.onetime.ts <= last_time) continue;
			if (e.u.onetime.ts > now) continue;
		} else {
			// Unknown code. TODO light up DEBUG led
			break;
		}
		
		// We got a match, run the action.
		e.act(e.arg);
	}

	last_time = now;
}

void serial_hello(uint8_t x) {
	serial_send('h');
	serial_send('e');
	serial_send('l');
	serial_send('l');
	serial_send('o');
	serial_send('0'+x);
}
