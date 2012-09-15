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

void effect(void) {
	int8_t fac = (ticks >> 4) % 2;

	clear_buffer();

	circle_shape(fac, 0, 0, 9, 13, MAX_INTENSITY);
	circle_shape(-fac, 0, 1, 8, 12, MAX_INTENSITY);
	circle_shape(0, fac, 2, 7, 11, MAX_INTENSITY);
	circle_shape(0, -fac, 3, 7, 10, MAX_INTENSITY);
	circle_shape(fac, 0, 4, 6, 9, MAX_INTENSITY);
	circle_shape(-fac, 0, 5, 3, 5, MAX_INTENSITY);
	circle_shape(0, fac, 6, 1, 3, MAX_INTENSITY);
	set_led(fac, 4, 7, MAX_INTENSITY);
}
