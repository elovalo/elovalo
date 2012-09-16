/*
 *  Copyright 2012 Elovalo project group.
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
#include <stdlib.h>
#include <stdint.h>

int8_t clamp(uint8_t x, uint8_t a, uint8_t b)
{
	return x < a? a: (x > b? b: x);
}

float fclamp(float x, float a, float b)
{
	return x < a? a: (x > b? b: x);
}

uint8_t randint(uint8_t min, uint8_t max)
{
	// could use some optimization
	return (rand() % max) + min;
}
