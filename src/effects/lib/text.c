/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Text utilities
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "font8x8_basic.h"
#include "utils.h"
#include "text.h"

void scroll_text(char text[], uint8_t x, uint16_t intensity, int16_t offset)
{
	uint8_t textLen = strlen(text);
	uint8_t spacing = 8; // Seems like a good pick for this charset

	offset %= (spacing + 1) * textLen;

	for(uint8_t i = 0; i < textLen; i++) {
		render_character(text[i], x, intensity, i * spacing + offset);
	}
}

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

