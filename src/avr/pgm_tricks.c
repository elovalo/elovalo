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

#include <stdint.h>
#include <avr/pgmspace.h>
#include "pgm_tricks.h"

bool is_pgm_ptr(const void *p)
{
	/* In ATmega328p the PROGMEM is not memory mapped but PROGMEM
	 * uses different start address anyway. Using that trick to
	 * find out whether the variable is in SRAM or in PROGMEM. See
	 * more info at
	 * http://uzebox.org/wiki/index.php?title=Emulator#Variables_in_flash_.28PROGMEM.29 */
	return (uint16_t)p >= 0x8000000;
}

void pgm_aware_copy(void *dst, const void *src, const int n)
{
	const uint8_t *p = (uint8_t*)src;
	const uint8_t *end = p+n;
	uint8_t *dst_u8 = (uint8_t*)dst;

	while (p < end) {
		*dst_u8++ = pgm_read_byte_near(p++);
	}
}
