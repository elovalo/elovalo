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

#include <avr/eeprom.h>
#include "cron.h"
#include "configuration.h"
#include "powersave.h"

#define EVERY_DAY 0x7f
#define END_OF_CRONTAB { .kind = END }

#define TIME(h,m) ((h)*60+(m))

/* Initial contents of EEPROM after programming: empty */
struct event eeprom_crontab[CRONTAB_SIZE] EEMEM = {
	END_OF_CRONTAB
};

uint32_t EEMEM eeprom_timezone = 0; // UTC by default
uint8_t EEMEM eeprom_effect = 0;
uint8_t EEMEM eeprom_playlist = 0;
uint8_t EEMEM eeprom_mode = 0;

void get_crontab_entry(struct event *p,uint8_t i)
{
	eeprom_read_block(p,eeprom_crontab+i,sizeof(struct event));
}

void truncate_crontab(uint8_t n) {
	if (n >= CRONTAB_SIZE) return;
	eeprom_update_byte((uint8_t*)&(eeprom_crontab[n].kind),(uint8_t)END);
}

void set_crontab_entry(struct event *p,uint8_t i)
{
	eeprom_update_block(p,eeprom_crontab+i,sizeof(struct event));
}

int32_t get_timezone(void)
{
	return eeprom_read_dword(&eeprom_timezone);
}

void set_timezone(int32_t tz)
{
	eeprom_update_dword(&eeprom_timezone,tz);
}

uint8_t read_effect(void)
{
	return eeprom_read_byte(&eeprom_effect);
}

void store_effect(uint8_t e)
{
	eeprom_update_byte(&eeprom_effect,e);
}

uint8_t read_playlist(void)
{
	return eeprom_read_byte(&eeprom_playlist);
}

void store_playlist(uint8_t p)
{
	eeprom_update_byte(&eeprom_playlist,p);
}

uint8_t read_mode(void)
{
	return eeprom_read_byte(&eeprom_mode);
}

void store_mode(uint8_t m)
{
	eeprom_update_byte(&eeprom_mode,m);
}
