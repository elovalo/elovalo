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

#ifndef FONT8x8_H_
#define FONT8x8_H_

#include <stdint.h>
#include <stdbool.h>

struct glyph {
	const uint8_t pixmap[8];
};

struct glyph_buf {
	uint16_t len;
	const struct glyph *buf[];
};

extern const struct glyph glyphs[];

/**
 * Get glyph for given UTF-8 encoded string. You must pass character
 * length in bytes to get correct result.
 */
const struct glyph *get_glyph_utf8(const char *p, uint8_t char_len);

/**
 * Shorthand for converting single ASCII character to glyph. Makes
 * life easier in countdown and such simple renderers.
 */
const struct glyph *get_glyph_ascii(const char c);

/**
 * Converts given UTF-8 string to glyph array.
 */
bool utf8_string_to_glyphs(const char *src, const uint16_t src_len, struct glyph_buf *dest);

/**
 * Convert given UTF-8 string to glyph array and stores it to EEPROM.
 */
bool utf8_string_to_eeprom(const char *src, const uint16_t src_len);

#endif /* FONT8x8_H_ */
