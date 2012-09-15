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

# pragma FLIP

#include "common.h"

//hmm ...
uint16_t hcsr04_get_distance_in_cm(void);

void effect(void)
{
	clear_buffer();
	uint8_t z = sensors.distance1 / 20;
	if (z >= LEDS_Z)
		z = LEDS_Z - 1;

	for(uint8_t x=0; x<2; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}

	z = sensors.distance2 / 20;
	if (z >= LEDS_Z)
		z = LEDS_Z - 1;

	for(uint8_t x=2; x<4; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}

	z = sensors.ambient_light / 32;
	for(uint8_t x=4; x<6; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
	
	z = sensors.sound_pressure_level / 32;
	for(uint8_t x=6; x<LEDS_X; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}

}
