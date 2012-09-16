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

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include "math.h"
#include "utils.h"

void circle_shape(int8_t xi, int8_t yi, int8_t zi, float rsq_min, float rsq_max, uint16_t intensity)
{
	xi -= LEDS_X / 2;
	yi -= LEDS_Y / 2;

	for(int8_t x = xi; x < LEDS_X + xi; x++) {
		for(int8_t y = yi; y < LEDS_Y + yi; y++) {
			float sq = x * x + y * y;

			if(rsq_min < sq && sq < rsq_max) {
				set_led(x - xi, y - yi, zi, MAX_INTENSITY);
			}
		}
	}
}

void fish_shape(uint8_t xi, uint8_t yi, uint8_t zi, uint16_t intensity)
{
	if(xi > 0) set_row(xi - 1, zi + 1, yi + 1, yi + 4, intensity);

	set_led(xi, yi, zi, intensity);
	set_row(xi, zi, yi + 2, yi + 4, intensity);
	set_row(xi, zi + 1, yi, yi + 5, intensity);
	set_row(xi, zi + 2, yi + 2, yi + 3, intensity);
	set_led(xi, yi, zi + 2, intensity);

	if(xi < LEDS_X - 1) set_row(xi + 1, zi + 1, yi + 1, yi + 4, intensity);
}

static void heart_layer(uint8_t x, uint16_t intensity);

void heart_shape(void)
{
	heart_layer(1, (float)MAX_INTENSITY / 100);
	heart_layer(2, (float)MAX_INTENSITY / 25);
	heart_layer(3, MAX_INTENSITY);
	heart_layer(4, MAX_INTENSITY);
	heart_layer(5, (float)MAX_INTENSITY / 25);
	heart_layer(6, (float)MAX_INTENSITY / 100);
}

static void heart_layer(uint8_t x, uint16_t intensity) {
	set_row(x, 0, 1, 2, intensity);
	set_row(x, 0, 5, 6, intensity);
	set_row(x, 1, 0, 7, intensity);
	set_row(x, 2, 0, 7, intensity);
	set_row(x, 3, 0, 7, intensity);
	set_row(x, 4, 1, 6, intensity);
	set_row(x, 5, 2, 5, intensity);
	set_row(x, 6, 3, 4, intensity);
}

void sphere_shape(float xi, float yi, float zi, float rsq_min, float rsq_max, float fac)
{
	for(float x = xi; x < LEDS_X + xi; x++) {
		for(float y = yi; y < LEDS_Y + yi; y++) {
			for(float z = zi; z < LEDS_Z + zi; z++) {
				float sq = x * x + y * y + z * z;

				if(rsq_min * fac < sq && sq < rsq_max * fac) {
					set_led(x - xi, y - yi, z - zi, MAX_INTENSITY);
				}
			}
		}
	}
}

/*
 * Bresenham's algorithm in 3D.
 *
 * Reference: ftp://ftp.isc.org/pub/usenet/comp.sources.unix/volume26/line3d
 */
void line(uint8_t x1, uint8_t y1, uint8_t z1, uint8_t x2, uint8_t y2, uint8_t z2,
	uint16_t intensity)
{
	uint8_t x, y, z, ax, ay, az;
	int8_t xd, yd, zd, dx, dy, dz, sx, sy, sz;

	dx = x2 - x1;
	dy = y2 - y1;
	dz = z2 - z1;

	ax = ABS(dx) << 1;
	ay = ABS(dy) << 1;
	az = ABS(dz) << 1;

	sx = ZSGN(dx);
	sy = ZSGN(dy);
	sz = ZSGN(dz);

	x = x1;
	y = y1;
	z = z1;

	if(ax >= MAX(ay, az)) { /* x dominant */
		yd = ay - (ax >> 1);
		zd = az - (ax >> 1);

		for(;;) {
			set_led(x, y, z, intensity);

			if(x == x2) return;

			if(yd >= 0) {
				y += sy;
				yd -= ax;
			}

			if(zd >= 0) {
				z += sz;
				zd -= ax;
			}

			x += sx;
			yd += ay;
			zd += az;
		}
	}
	else if(ay >= MAX(ax, az)) { /* y dominant */
		xd = ax - (ay >> 1);
		zd = az - (ay >> 1);

		for(;;) {
			set_led(x, y, z, intensity);

			if(y == y2) return;

			if(xd >= 0) {
				x += sx;
				xd -= ay;
			}

			if(zd >= 0) {
				z += sz;
				zd -= ay;
			}

			y += sy;
			xd += ax;
			zd += az;
		}
	}
	else if(az >= MAX(ax, ay)) { /* z dominant */
		xd = ax - (az >> 1);
		yd = ay - (az >> 1);

		for(;;) {
			set_led(x, y, z, intensity);

			if(z == z2) return;

			if(xd >= 0) {
				x += sx;
				xd -= az;
			}

			if(yd >= 0) {
				y += sy;
				yd -= az;
			}

			z += sz;
			xd += ax;
			yd += ay;
		}
	}
}

void cube_shape(uint8_t x1, uint8_t y1, uint8_t z1, uint8_t x2, uint8_t y2, uint8_t z2,
	uint16_t intensity)
{
	line(x1, y1, z1, x1, y1, z2, intensity);
	line(x1, y1, z1, x2, y1, z1, intensity);
	line(x1, y1, z1, x1, y2, z1, intensity);

	line(x2, y2, z2, x2, y2, z1, intensity);
	line(x2, y2, z2, x2, y1, z2, intensity);
	line(x2, y2, z2, x1, y2, z2, intensity);

	line(x1, y2, z1, x2, y2, z1, intensity);
	line(x1, y2, z1, x1, y2, z2, intensity);
	line(x2, y1, z1, x2, y1, z2, intensity);
	line(x2, y1, z1, x2, y2, z1, intensity);
	line(x1, y1, z2, x1, y2, z2, intensity);
	line(x1, y1, z2, x2, x1, z2, intensity);
}
