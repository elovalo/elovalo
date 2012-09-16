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

const uint8_t particles_xyz_len = 5;
xyz_t particles_xyz[5];

void init(void)
{
	for(uint8_t i = 0; i < particles_xyz_len; i++) {
		xyz_t p = {
			.x=randint(0, LEDS_X),
			.y=randint(0, LEDS_Y),
			.z=randint(0, LEDS_Z)
		};

		particles_xyz[i] = p;
	}

	clear_buffer();
}
void effect(void)
{
	if(ticks % 50) return;

	clear_buffer();

	for(uint8_t i = 0; i < particles_xyz_len; i++) {
		xyz_t p = particles_xyz[i];

		set_led(p.x, p.y, p.z, MAX_INTENSITY);

		p.x = clamp(p.x + randint(-1, 1), 0, LEDS_X - 1);
		p.y = clamp(p.y + randint(-1, 1), 0, LEDS_Y - 1);
		p.z = clamp(p.z + randint(-1, 1), 0, LEDS_Z - 1);

		particles_xyz[i] = p;
	}
}
