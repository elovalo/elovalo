/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
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

uint16_t worm_pos[3];
uint16_t worm_dir;
int worm_speed;

void init(void)
{
	worm_pos[0] = 4;
	worm_pos[1] = 4;
	worm_pos[2] = 4;
	worm_dir = 0;
	worm_speed = 1;

	clear_buffer();
}
void effect(void)
{
	set_led(worm_pos[0], worm_pos[1], worm_pos[2], MAX_INTENSITY);

	// Relies on the fact that it's a cube
	// Not entirely fool proof but probably good enough
	int new_pos = worm_pos[worm_dir] + worm_speed;
	if(new_pos < 0 || new_pos >= LEDS_X || randint(0, 10) > 7) {
		worm_dir = worm_dir + 1 > 2? 0: worm_dir + 1;
		worm_speed = -worm_speed;
	}

	worm_pos[worm_dir] += worm_speed;
}
