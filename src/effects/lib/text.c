/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Text utilities
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "font8x8_basic.h"
#include "utils.h"
#include "text.h"
#include "../../pgmspace.h"

static const uint8_t spacing = 8; // Seems like a good pick for this charset

void scroll_text(const char text[], uint8_t x, int16_t offset, render_t f)
{
	// text format is ZCL octet string, where the length is at byte 0
	uint8_t text_len = pgm_get(*text++,byte);
	int16_t base_pos = (LEDS_X+1)-offset;
	int16_t i = -base_pos / spacing;
	uint16_t pos = i*spacing + base_pos;

	// Render only two characters which fit to screen

	if (i >= 0 && i < text_len) {
		const char c = pgm_get(text[i],byte);
		render_character(c, x, pos, f);
	}

	if (i+1 >= 0 && i+1<text_len) {
		const char c = pgm_get(text[i+1],byte);
		render_character(c, x, pos+spacing, f);
	}
}

void render_character(uint8_t index, uint8_t x, int16_t offset, render_t f)
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

			if(set) f(x, y, z);
		}
	}
}

void render_yz(uint8_t x, uint8_t y, uint8_t z) {
	set_led(x, y, z, MAX_INTENSITY);
}

