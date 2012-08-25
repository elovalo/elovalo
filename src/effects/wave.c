#include "common.h"

/*
 * 6000, FLIP
 */
TWOD(wave_effect)
{
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;

	return scaler * (2 + sin((float)x * 50 + (float)ticks / 15));
}
