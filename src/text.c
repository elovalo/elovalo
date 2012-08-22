/**
 * Text utilities
 */

#include <stdlib.h>
#include <stdint.h>
#include "lib/font8x8_basic.h"
#include "effect_utils.h"
#include "text.h"

/**
 * Scrolls given text
 */
void scroll_text(char text[], uint8_t x, uint16_t intensity, int16_t offset)
{
	// TODO: figure out how to deal with multiple chars
	render_character(text[0], x, intensity, offset);
}

/**
 * Renders character (index from font8x8_basic) at x with intensity.
 */
void render_character(uint8_t index, uint8_t x, uint16_t intensity, int16_t offset)
{
	uint8_t *bitmap = font8x8_basic[index];

	for(uint8_t y = 0; y < 8; y++) {
		for(uint8_t z = 0; z < 8; z++) {
			int8_t loc = y - offset;

			if(loc < 0 || loc >= 8) continue;

			int8_t set = bitmap[z] & 1 << loc;

			if(set) set_led_8_8_12(x, y, z, intensity);
		}
	}
}

