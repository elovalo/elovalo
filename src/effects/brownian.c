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
	xyz_t xyz;
} vars;

void init(void)
{
	vars.xyz = (xyz_t){
		.x = LEDS_X / 2,
		.y = LEDS_Y / 2,
		.z = LEDS_Z / 2
	};

	clear_buffer();
}
void effect(void)
{
	set_led(vars.xyz.x, vars.xyz.y, vars.xyz.z, MAX_INTENSITY);

	vars.xyz = (xyz_t){
		.x = clamp(vars.xyz.x + randint(-2, 2), 0, LEDS_X - 1),
		.y = clamp(vars.xyz.y + randint(-2, 2), 0, LEDS_Y - 1),
		.z = clamp(vars.xyz.z + randint(-2, 2), 0, LEDS_Z - 1)
	};
}
