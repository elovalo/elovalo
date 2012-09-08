#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include "utils.h"

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
					set_led(x - zi, y - yi, z - zi, MAX_INTENSITY);
				}
			}
		}
	}
}

static void swap(int8_t *a, int8_t *b);

/*
 * Bresenham's algorithm in 3D.
 *
 * Reference: http://www.cobrabytes.com/index.php?topic=1150.0
 */
void line(int8_t x1, int8_t y1, int8_t z1, int8_t x2, int8_t y2, int8_t z2,
		uint16_t intensity)
{
	uint8_t delta_x, delta_y, delta_z, x, y, z, swap_xy, swap_xz;
	int8_t drift_xy, drift_xz, cx, cy, cz;

	if(x1 > x2) {
		swap(&x1, &x2);
		swap(&y1, &y2);
		swap(&z1, &z2);
	}

	swap_xy = abs(y2 - y1) > abs(x2 - x1);
	if(swap_xy) {
		swap(&x1, &y1);
		swap(&x2, &y2);
	}

	swap_xz = abs(z2 - z1) > abs(x2 - x1);
	if(swap_xz) {
		swap(&x1, &z1);
		swap(&x2, &z2);
	}

	delta_x = abs(x2 - x1);
	delta_y = abs(y2 - y1);
	delta_z = abs(z2 - z1);

	drift_xy = drift_xz = delta_x / 2;

	y = y1;
	z = z1;

	// problematic if x1 == x2
	for(x = x1; x < x2; x++) {
		cx = x;
		cy = y;
		cz = z;

		set_led(cx, cy, cz, intensity);

		if(swap_xz) swap(&cx, &cz);
		if(swap_xy) swap(&cx, &cy);

		drift_xy -= delta_y;
		drift_xz -= delta_z;

		if(drift_xy < 0) {
			y++;;
			drift_xy += delta_x;
		}

		if(drift_xz < 0) {
			z++;
			drift_xz += delta_x;
		}
	}
}

static void swap(int8_t *a, int8_t *b) {
	int8_t tmp = *a;
	*a = *b;
	*b = tmp;
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
}
