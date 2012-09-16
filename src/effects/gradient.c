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

#pragma FLIP

#include "common.h"

void effect(void)
{
	float fac;
	int8_t cur = (ticks >> 5) % 8;

	clear_buffer();

	for(int8_t i = 0; i < LEDS_X; i++) {
		for(uint8_t j = 0; j < LEDS_Z; j++) {
			fac = j <= cur? (cur - j) * 0.1: 0.0;

			set_row(i, LEDS_Z - 1 - j, 0, 7, MAX_INTENSITY * fac);
		}
	}
}
