# pragma FLIP

#include "common.h"

XY(effect)
{
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;
	uint16_t i = scaler * (2 + sin((float)x * 50 + (float)ticks / 15));

	// XXX: clear_buffer screws up set_z!
	clear_buffer();
	set_z(x, y, i);
}
