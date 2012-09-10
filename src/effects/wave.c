# pragma FLIP

#include "common.h"

XY(effect)
{
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;
	uint16_t i = scaler * (2 + sin((float)x * 50 + (float)ticks / 15));

	set_z(x, y, i);
}
