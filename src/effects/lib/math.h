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

// math utils from
// ftp://ftp.isc.org/pub/usenet/comp.sources.unix/volume26/line3d
#define MAX(a,b) (((a)>(b))?(a):(b))
#define ABS(a) (((a)<0) ? -(a) : (a))

/* take sign of a, either -1, 0, or 1 */
#define ZSGN(a) (((a)<0) ? -1 : (a)>0 ? 1 : 0)

typedef struct {
	uint8_t x;
	uint8_t y;
	uint8_t z;
} xyz_t;

uint8_t randint(uint8_t min, uint8_t max);
uint8_t clamp(uint8_t a, uint8_t min, uint8_t max);
float fclamp(float a, float min, float max);
