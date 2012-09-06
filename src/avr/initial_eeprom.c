#include <avr/eeprom.h>
#include "schedule.h"
#include "initial_eeprom.h"

#define EVERY_DAY 0x7f

#define TIME(h,m) ((h)*60+(m))
#define END_OF_SCHEDULE { .kind = END }

struct event eeprom_schedule[SCHEDULE_SIZE] EEMEM = {
	{ .kind = WEEKLY,
	  .act = CUBE_START,
	  .u = { .weekly = { .weekdays = EVERY_DAY,
			     .minutes = TIME(2,0) },
		}
	},
	{ .kind = WEEKLY,
	  .act = CUBE_SHUTDOWN,
	  .u = { .weekly = { .weekdays = EVERY_DAY,
			     .minutes = TIME(19,0) },
		}
	},
	END_OF_SCHEDULE
};

