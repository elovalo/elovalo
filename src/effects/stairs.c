#include "common.h"

/*
 * 100, NO_FLIP
 */
void stairs_init(void)
{
	for(uint8_t i = 0; i < 8; i++) {
		set_row(i, i, 0, 7, MAX_INTENSITY);
	}
}
