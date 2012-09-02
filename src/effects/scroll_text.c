# pragma FLIP

#include "common.h"

void effect(void)
{
	clear_buffer();
	scroll_text("elovalo", 3, MAX_INTENSITY, -(ticks >> 2));
}
