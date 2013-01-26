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
 * Timezone functions on desktop platform. TODO implement those if
 * needed. These affect the way clock is shown in effects. Currently
 * time zone in effect exporting is hard-wired to UTC and can not be
 * changed.
 */

#include <stdint.h>

/**
 * Gets timezone as second offset.
 */
int32_t get_timezone(void)
{
	return 0; // Hard-wired to UTC
}

/**
 * Set timezone as second offset and writes it to EEPROM.
 */
void set_timezone(int32_t tz)
{
	// NOT IMPLEMENTED
}
