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
void get_crontab_entry(struct event *p,uint8_t i)
{
	eeprom_read_block(p,eeprom_crontab+i,sizeof(struct event));
}

/**
 * Truncates crontab to contain n elements. When n is zero, it
 * effectively clears the crontab.
 */
void truncate_crontab(uint8_t n) {
	if (n >= CRONTAB_SIZE) return;
	eeprom_update_byte((uint8_t*)&(eeprom_crontab[n].kind),(uint8_t)END);
}

/**
 * Sets crontab entry. If you append to crontab, remember to
 * truncate_crontab() afterwards to ensure there are no "dangling"
 * objects left there. Do not leave gaps to crontab. If you need to
 * clear an element, use kind of EMPTY.
 */
void set_crontab_entry(struct event *p,uint8_t i)
{
	eeprom_update_block(p,eeprom_crontab+i,sizeof(struct event));
}
