#include "common.h"

/*
 * FLIP
 */
TWOD(effect)
{
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;

	return scaler * (2 + sin((float)x / 2 + (float)ticks / 15) + sin((float)y / 2 + (float)ticks / 30));
}
