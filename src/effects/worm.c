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
