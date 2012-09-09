#include <avr/eeprom.h>
#include "cron.h"
#include "configuration.h"
#include "powersave.h"

#define EVERY_DAY 0x7f
#define END_OF_CRONTAB { .kind = END }

#define TIME(h,m) ((h)*60+(m))

/* Initial contents of EEPROM after programming. In future this may be
 * empty but currently it holds schedule for daily powersave from
 * 02:00–19:00. */
struct event eeprom_crontab[CRONTAB_SIZE] EEMEM = {
	{ .kind = WEEKLY,
	  .act = &cube_shutdown,
	  .u = { .weekly = { .weekdays = EVERY_DAY,
			     .minutes = TIME(2,0) },
		}
	},
	{ .kind = WEEKLY,
	  .act = &cube_start,
	  .u = { .weekly = { .weekdays = EVERY_DAY,
			     .minutes = TIME(19,0) },
		}
	},
	END_OF_CRONTAB
};

/**
 * Gets single crontab entry from crontab (stored on EEPROM).
 */
void get_crontab_entry(struct event *e,uint8_t i)
{
	eeprom_read_block(&e,eeprom_crontab+i,sizeof(struct event));
}
