#include "common.h"

/*
 * FLIP
 */
void effect(void)
{
	clear_buffer();
	scroll_text("elovalo", 3, MAX_INTENSITY, -(ticks >> 2));
}
