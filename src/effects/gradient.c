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

#pragma FLIP

#include "common.h"

void effect(void)
{
	float fac;
	uint8_t cur = (ticks >> 2) % 8;

	clear_buffer();

	for(uint8_t i = 0; i < LEDS_X; i++) {
		if(i > cur) fac = 0.0;
		else fac = (cur - i) * 0.1;

		for(uint8_t j = 0; j < LEDS_Z; j++) {
			set_row(i, j, 0, 7, MAX_INTENSITY * fac);
		}
	}
}