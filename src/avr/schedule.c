/**
 * Functions for accessing schedule
 */

#include <stdlib.h>
#include <avr/eeprom.h>
#include "../pgmspace.h"
#include "timer.h"
#include "schedule.h"
#include "powersave.h"
#include "serial.h" // For temporary debug code

#define EEPROM_SCHEDULE_POS (struct event *)1
#define SECS_IN_DAY ((time_t)60*60*24)

void serial_hello(void);

const action_t actions[] PROGMEM = { 
	cube_shutdown,
	cube_start,
	serial_hello
};

enum event_kind {
	DAILY   = 0x00, // Weekly caledar
	ONETIME = 0x01, // One-time event
	EMPTY   = 0xfe, // Not set, go to next
	END     = 0xff  // End of schedule
};

struct event {
	enum event_kind kind;
	uint8_t act; // Action to run
	union {
		struct {
			uint8_t weekdays; // Thursday is LSB
			int16_t minutes;  // Minutes from midnight, UTC
		} daily;
		struct {
			time_t ts;   // Timestamp of execution
		} onetime;
	} u;
};

void periodic_check() {
	static time_t last_time = 0;

	// Some calculations. TODO if interrupts are enabled, do not use this unsafe thingy
	time_t start_of_day = unsafe_time(NULL) / SECS_IN_DAY * SECS_IN_DAY;
	int16_t last_min = (last_time < start_of_day)
		? -1
		: (last_time-start_of_day)/60;
	int16_t now_min = (unsafe_time(NULL) - start_of_day)/60;
	uint8_t weekday = (uint8_t)(unsafe_time(NULL) / SECS_IN_DAY % 7);

	struct event *eeprom_p = EEPROM_SCHEDULE_POS;
	while (eeprom_p++) {
		struct event e;
		eeprom_read_block(&e,eeprom_p,sizeof(struct event));
		
		// Doing the checks
		if (e.kind == END) break;
		if (e.kind == EMPTY) continue;
		if (e.kind == DAILY) {
			// If not this day, skip
			if (!(e.u.daily.weekdays & (1 << weekday))) continue;
			// If haven't happened, skip
			if (e.u.daily.minutes <= last_min) continue;
			if (e.u.daily.minutes > now_min) continue;
		}
		else if (e.kind == ONETIME) {
			if (e.u.onetime.ts <= last_time) continue;
			if (e.u.onetime.ts > unsafe_time(NULL)) continue;
		}
		
		// We got a match, run the action.
		action_t act = (action_t)pgm_get(actions[e.act],word);
		act();
	}

	last_time = unsafe_time(NULL);
}

void serial_hello(void) {
	serial_send('h');
	serial_send('e');
	serial_send('l');
	serial_send('l');
	serial_send('o');
}
