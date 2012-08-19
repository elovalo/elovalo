/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 */

#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include "env.h"
#include "effects.h"
#include "effect_utils.h"

static void init_brownian(void);
static void init_worm(void);

const effect_t effects[] = {
	{ "brownian", &init_brownian, &effect_brownian, 100, 0 },
	{ "sine", NULL, &effect_sine, 600, 1 },
	{ "wave", NULL, &effect_wave, 600, 1 },
	{ "sphere", NULL, &effect_sphere, 100, 1 },
	{ "worm", &init_worm, &effect_worm, 600, 0 },
	{ "const", NULL, &effect_constant, 100, 1 },
	{ "layers", NULL, &effect_layers_tester, 100, 1 }
};

const uint8_t effects_len = sizeof(effects) / sizeof(effect_t);

/**
 * Brownian particle. Starts near center and then accumulates.
 */
uint8_t brown_x;
uint8_t brown_y;
uint8_t brown_z;
static void init_brownian(void)
{
	brown_x = (uint8_t)(LEDS_X / 2);
	brown_y = (uint8_t)(LEDS_Y / 2);
	brown_z = (uint8_t)(LEDS_Z / 2);

	clear_buffer();
}
void effect_brownian(void)
{
	set_led(brown_x, brown_y, brown_z, (1<<GS_DEPTH) - 1);

	brown_x = clamp(brown_x + randint(-2, 2), 0, LEDS_X - 1);
	brown_y = clamp(brown_y + randint(-2, 2), 0, LEDS_Y - 1);
	brown_z = clamp(brown_z + randint(-2, 2), 0, LEDS_Z - 1);
}

/**
 * Plot 2-dimensional sine waves which are moving. The parameters are
 * not tuned, this is just taken from my head.
 */
TWOD(effect_sine)
{
	const float sine_scaler = (float)MAX_2D_PLOT_INTENSITY/4;

	return sine_scaler * (2 + sin((float)x/2+(float)ticks/15) + sin((float)y/2+(float)ticks/30));
}

/**
 * Plot 1-dimensional sine waves.
 */
TWOD(effect_wave)
{
	const float sine_scaler = (float)MAX_2D_PLOT_INTENSITY / 4;

	return sine_scaler * (2 + sin((float)x * 50 + (float)ticks / 15));
}

/**
 * Plot sphere.
 */
void effect_sphere(void)
{
	float a = -3.5;
	float rsq_max = 16;
	float rsq_min = 10;
	int step = 50;
	int f = 1000;
	float fac = fmax(ceil((ticks % f) / step) * step / f, 0.2); // TODO bebraw: scale ticks

	clear_buffer();

	for (float x = a; x < LEDS_X + a; x++) {
		for (float y = a; y < LEDS_Y + a; y++) {
			for (float z = a; z < LEDS_Z + a; z++) {
				float sq = x * x + y * y + z * z;

				if(rsq_min * fac < sq && sq < rsq_max * fac) {
					set_led(x - a, y - a, z - a, (1<<GS_DEPTH) - 1);
				}
			}
		}
	}
}

/**
 * Plot worm.
 */
uint16_t worm_pos[3];
uint16_t worm_dir;
int worm_speed;
static void init_worm(void)
{
	worm_pos[0] = 4;
	worm_pos[1] = 4;
	worm_pos[2] = 4;
	worm_dir = 0;
	worm_speed = 1;

	clear_buffer();
}
void effect_worm(void)
{
	set_led(worm_pos[0], worm_pos[1], worm_pos[2], (1<<GS_DEPTH) - 1);

	// Relies on the fact that it's a cube
	// Not entirely fool proof but probably good enough
	int new_pos = worm_pos[worm_dir] + worm_speed;
	if(new_pos < 0 || new_pos >= LEDS_X || randint(0, 10) > 7) {
		worm_dir = worm_dir + 1 > 2? 0: worm_dir + 1;
		worm_speed = -worm_speed;
	}

	worm_pos[worm_dir] += worm_speed;
}

/**
 * Constant plot. Helps to spot errors in drawing code. Replace this
 * with something more useful in the future.
 */
TWOD(effect_constant)
{
	return 10000;
}

/**
 * Simple test function which draws voxel layers.
 */
void effect_layers_tester(void)
{
	clear_buffer();
	uint8_t z = (ticks >> 1 % LEDS_Z);

	for (uint8_t x=0; x<LEDS_X; x++) {
		for (uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, (1<<GS_DEPTH) - 1);
		}
	}
}
