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

#include "common.h"

struct {
	xyz_t xyz[10];
} vars;

static const uint8_t starfield_xyz_len = 10;

void init(void)
{
	for(uint8_t i = 0; i < starfield_xyz_len; i++) {
		vars.xyz[i] = (xyz_t){
			.x = randint(0, LEDS_X),
			.y = randint(0, LEDS_Y),
			.z = randint(0, LEDS_Z)
		};
	}

	clear_buffer();
}
void effect(void)
{
	clear_buffer();

	for(uint8_t i = 0; i < starfield_xyz_len; i++) {
		xyz_t xyz = vars.xyz[i];

		set_led(xyz.x, xyz.y, xyz.z, MAX_INTENSITY);

		uint8_t y = xyz.y;

		y++;

		if(y >= LEDS_Y) y = 0;

		vars.xyz[i].y = y;
	}
}
