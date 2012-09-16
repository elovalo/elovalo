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
#define CRONTAB_SIZE 10

#include "cron.h"

/**
 * Gets single crontab entry from crontab (stored on EEPROM).
 */
void get_crontab_entry(struct event *p, uint8_t i);

/**
 * Truncates crontab to contain n elements. When n is zero, it
 * effectively clears the crontab.
 */
void truncate_crontab(uint8_t n);

/**
 * Sets crontab entry. If you append to crontab, remember to
 * truncate_crontab() afterwards to ensure there are no "dangling"
 * objects left there. Do not leave gaps to crontab. If you need to
 * clear an element, use kind of EMPTY.
 */
void set_crontab_entry(struct event *p,uint8_t i);
