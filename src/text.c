/**
 * Text utilities
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "lib/font8x8_basic.h"
#include "effect_utils.h"
#include "text.h"

/**
 * Scrolls given text
 */
void scroll_text(char text[], uint8_t x, uint16_t intensity, int16_t offset)
{
	uint8_t textLen = strlen(text);
	uint8_t spacing = 7; // 7 seems like a good pick for this charset

	offset %= (spacing + 1) * textLen;

	for(uint8_t i = 0; i < textLen; i++) {
		render_character(text[i], x, intensity, i * spacing + offset);
	}
}

/**
 * Renders character (index from font8x8_basic) at x with intensity.
 */
void render_character(uint8_t index, uint8_t x, uint16_t intensity, int16_t offset)
{
	uint8_t bitmap[8];

	// Read character from the font in PROGMEM
	for (uint8_t i=0; i<8; i++) {
		bitmap[i] = pgm_get(font8x8_basic[index][i],byte);
	}

	for(uint8_t y = 0; y < 8; y++) {
		for(uint8_t z = 0; z < 8; z++) {
			int8_t loc = y - offset;

			if(loc < 0 || loc >= 8) continue;

			int8_t set = bitmap[z] & 1 << loc;

			if(set) set_led_8_8_12(x, y, z, intensity);
		}
	}
}

