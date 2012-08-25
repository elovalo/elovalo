#include "common.h"
#include "../text.h"


/*
 * 100, FLIP
 */
void scroll_text_effect(void)
{
	clear_buffer();
	scroll_text("elovalo", 3, MAX_INTENSITY, -(ticks >> 2));
}
