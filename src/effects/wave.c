# pragma FLIP

#include "common.h"

XZ(effect)
{
	float scaler = (float)MAX_2D_PLOT_INTENSITY / 4;

	return scaler * (2 + sin((float)x * 50 + (float)ticks / 15));
}
