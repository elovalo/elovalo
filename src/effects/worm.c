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
	uint16_t worm_pos[3];
	uint16_t worm_dir;
	int worm_speed;
} vars;

void init(void)
{
	vars.worm_pos[0] = 4;
	vars.worm_pos[1] = 4;
	vars.worm_pos[2] = 4;
	vars.worm_dir = 0;
	vars.worm_speed = 1;

	clear_buffer();
}
void effect(void)
{
	set_led(vars.worm_pos[0], vars.worm_pos[1], vars.worm_pos[2], MAX_INTENSITY);

	// Relies on the fact that it's a cube
	// Not entirely fool proof but probably good enough
	int new_pos = worm_pos[vars.worm_dir] + vars.worm_speed;
	if(new_pos < 0 || new_pos >= LEDS_X || randint(0, 10) > 7) {
		vars.worm_dir = vars.worm_dir + 1 > 2? 0: vars.worm_dir + 1;
		vars.worm_speed = -vars.worm_speed;
	}

	vars.worm_pos[vars.worm_dir] += vars.worm_speed;
}
