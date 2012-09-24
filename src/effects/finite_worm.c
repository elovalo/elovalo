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

#define WORM_LENGTH 10
#pragma MAX_FPS 20

struct {
	uint16_t pos[3];
	uint16_t dir;
	int8_t speed;

	uint16_t prev_dirs[10]; // FIXME WORM_LENGTH
	int8_t prev_speeds[10]; // FIXME WORM_LENGTH
	uint8_t prev_dir_i;
} vars;

void init(void)
{
	vars.pos[0] = 4;
	vars.pos[1] = 4;
	vars.pos[2] = 4;
	vars.dir = 0;
	vars.speed = 1;

	memset(vars.prev_dirs, 10, WORM_LENGTH);
	vars.prev_dir_i = 0;

	clear_buffer();
}
void effect(void)
{
	clear_buffer();

	set_led(vars.pos[0], vars.pos[1], vars.pos[2], MAX_INTENSITY);

	// Backtracing
	if(vars.prev_dirs[0] != 10) {
		int8_t i;
		uint8_t j;
		uint16_t tmp_pos[3] = {vars.pos[0], vars.pos[1], vars.pos[2]};

		for(i = vars.prev_dir_i - 1, j = 0; j < WORM_LENGTH; i--, j++) {
			if(i == -1) i = WORM_LENGTH-1;

			tmp_pos[vars.prev_dirs[i]] -= vars.prev_speeds[i];

			set_led(tmp_pos[0], tmp_pos[1], tmp_pos[2], MAX_INTENSITY);
		}
	}

	// Relies on the fact that it's a cube
	// Not entirely fool proof but probably good enough
	int new_pos = vars.pos[vars.dir] + vars.speed;
	if(new_pos < 0 || new_pos >= LEDS_X || randint(0, 10) > 7) {
		vars.dir = vars.dir + 1 > 2? 0: vars.dir + 1;
		vars.speed = -vars.speed;
	}

	vars.prev_dirs[vars.prev_dir_i] = vars.dir;
	vars.prev_speeds[vars.prev_dir_i] = vars.speed;

	if(vars.prev_dir_i < WORM_LENGTH-1) {
		vars.prev_dir_i++;
	}
	else {
		vars.prev_dir_i = 0;
	}

	vars.pos[vars.dir] += vars.speed;
}
