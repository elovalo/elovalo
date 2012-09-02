/**
 * Functions for accessing schedule
 */

#include "../pgmspace.h"
#include "timer.h"
#include "schedule.h"
#include "powersave.h"
#include "serial.h" // For temporary debug code

#define EEPROM_SCHEDULE_POS (event_t*)1
#define MINUTE_NOT_SET 0xffff
#define SECS_IN_DAY (60*60*24)

void serial_hello(void);

const action_t actions[] PROGMEM = { 
	cube_shutdown,
	cube_start,
	serial_hello
};

typedef struct {
	enum kind {
		DAILY   = 0x00, // Weekly caledar
		ONETIME = 0x01, // One-time event
		EMPTY   = 0xfe, // Not set, go to next
		END     = 0xff  // End of schedule
	};
	union {
		struct {
			uint8_t weekdays; // Thursday is LSB
			int16_t minutes;  // Minutes from midnight, UTC
			uint8_t act;      // Action to run
		} daily;
		struct {
			time_t ts;   // Timestamp of execution
			uint8_t act; // Index of function to execute
		} onetime;
	} u;
} event_t;

void periodic_check() {
	static time_t last_time = 0;

	// Some calculations
	time_t start_of_day = unsafe_time(NULL) / SECS_IN_DAY * SECS_IN_DAY;
	int16_t last_min = (last_time < start_of_day)
		? -1
		: (last_time-start_of_day)/60;
	int16_t now_min = (unsafe_time(NULL) - start_of_day)/60;
	uint8_t weekday = unsafe_time(NULL) / SECS_IN_DAY % 7;

	event_t *el = EEPROM_SCHEDULE_POS;
	while (el++) {
		event_t event = eeprom_read_event(el);
		if (event.kind == END) break;
		if (event.kind == EMPTY) continue;
		if (event.kind == DAILY) {
			// If not this day, skip
			if (!(event.weekdays & (1 << weekday))) continue;
			// If haven't happened, skip
			if (event.minutes <= last_min) continue;
			if (event.minutes > now_min) continue;
			// Otherwise, run.
			(action_t)pgm_get(actions[event.act],word)();
	}

void serial_hello(void) {
	serial_send('h');
	serial_send('e');
	serial_send('l');
	serial_send('l');
	serial_send('o');
}
