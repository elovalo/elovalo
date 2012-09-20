/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Functions for accessing schedule
 */

#include <stdbool.h>
#include <stdlib.h>
#include "../pgmspace.h"
#include "cron.h"
#include "powersave.h"
#include "configuration.h"
#include "tlc5940.h"
#include "serial.h" // For temporary debug code

#define SECS_IN_DAY ((time_t)60*60*24)

// The info table for serial port access

PROGMEM const char key_cube_shutdown[] = "off";
PROGMEM const char key_cube_start[] = "on";
PROGMEM const char key_serial_hello[] = "hello";
PROGMEM const char key_dimming[] = "intensity";

PROGMEM const char s_cube_shutdown[] = "Power-off the cube part";
PROGMEM const char s_cube_start[] = "Power-on the cube part";
PROGMEM const char s_serial_hello[] = "Say \"hello\" to serial console";
PROGMEM const char s_serial_hello_arg[] = "Number to write after \"hello\"";
PROGMEM const char s_dimming[] = "Set cube intensity";
PROGMEM const char s_dimming_arg[] = "Intensity between 0 and 255";

const struct action_info cron_actions[] PROGMEM = { 
	{ &cube_shutdown, key_cube_shutdown, s_cube_shutdown, NULL },
	{ &cube_start, key_cube_start, s_cube_start, NULL },
	{ &serial_hello, key_serial_hello, s_serial_hello, s_serial_hello_arg},
	{ &tlc5940_set_dimming, key_dimming, s_dimming, s_dimming_arg}
};

const uint8_t cron_actions_len = sizeof(cron_actions)/sizeof(struct action_info);

bool is_event_valid(struct event *e)
{
	// Validate kind. END is not there for a reason.
	if (e->kind != WEEKLY &&
	    e->kind != ONETIME &&
	    e->kind != EMPTY) return false;

	// Validate act
	return is_action_valid(e->act);
}

bool is_action_valid(action_t act)
{
	// Validate act
	for (uint8_t i=0; i<cron_actions_len; i++) {
		action_t x = (action_t)pgm_get(cron_actions[i].act, word);
		if (act == x) return true;
	}
	return false;
}

void run_cron(const time_t now) {
	static time_t last_time = 0;

	// Do not break when clock is set to past
	if (last_time > now) last_time=now;

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
