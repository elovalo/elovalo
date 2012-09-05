# pragma FLIP

#include "common.h"

void effect(void)
{
	clear_buffer();
	set_led(0, 0, (ticks >> 2) & 7, MAX_INTENSITY);
	set_led(0, 7, (ticks >> 2) & 7, MAX_INTENSITY);
	set_led(7, 0, (ticks >> 2) & 7, MAX_INTENSITY);
	set_led(7, 7, (ticks >> 2) & 7, MAX_INTENSITY);
	//scroll_text("Elovalo loves U", 7, MAX_INTENSITY, 10-(ticks >> 3));
}
