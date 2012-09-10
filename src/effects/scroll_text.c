# pragma FLIP

#include "common.h"

void effect(void)
{
	clear_buffer();
	scroll_text("Elovalo loves U", 7, MAX_INTENSITY, 10-(ticks >> 3));
}
