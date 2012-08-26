#include "common.h"

/*
 * FLIP
 */
void effect(void)
{
	float a = -3.5 - 3 * (float)(ticks % 26 - 13) / 13;

	sphere_shape(a, a, a, 8, 16, 0.2);
}
