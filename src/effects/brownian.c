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

#include "common.h"

uint8_t brown_x;
uint8_t brown_y;
uint8_t brown_z;

void init(void)
{
	brown_x = (uint8_t)(LEDS_X / 2);
	brown_y = (uint8_t)(LEDS_Y / 2);
	brown_z = (uint8_t)(LEDS_Z / 2);

	clear_buffer();
}
void effect(void)
{
	set_led(brown_x, brown_y, brown_z, MAX_INTENSITY);

	brown_x = clamp(brown_x + randint(-2, 2), 0, LEDS_X - 1);
	brown_y = clamp(brown_y + randint(-2, 2), 0, LEDS_Y - 1);
	brown_z = clamp(brown_z + randint(-2, 2), 0, LEDS_Z - 1);
}
