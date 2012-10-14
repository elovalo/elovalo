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

#include "font8x8.h"
#include "../../pgmspace.h"
#include "font8x8_generated.h"

#define BIT_NOT_SET(x,y) (!((x) & (1 << (y))))

const struct glyph *get_glyph_utf8(const char *p, uint8_t char_len)
{
	if (char_len > MAX_WORD_LENGTH || char_len < MIN_WORD_LENGTH)
		return glyphs; // return anything

	const int key = hash(p,char_len);

	if (key > MAX_HASH_VALUE || key < 0)
		return glyphs; // return anything

	/* Allow false negatives - do not even validate because we
	 * don't have any strings for comparison */
	return &glyphs[key];
}

const struct glyph *get_glyph_ascii(const char c)
{
	return get_glyph_utf8(&c,1);
}

bool utf8_string_to_glyphs(const char *src, const uint16_t src_len, struct glyph_buf *dest)
{
	const char *end = src+src_len;
	const struct glyph **dest_p = dest->buf;

	while (src < end) {
		// Is there enough room to store one glyph
		if (dest_p >= dest->buf + dest->len)
			return false;

		char x = *src;
		uint8_t bytes;
		
		if (BIT_NOT_SET(x,7))
			bytes = 1;
		else if (BIT_NOT_SET(x,6))
			return false; // UTF-8 decoding error
		else if (BIT_NOT_SET(x,5))
			bytes = 2;
		else if (BIT_NOT_SET(x,4))
			bytes = 3;
		else if (BIT_NOT_SET(x,3))
			bytes = 4;
		else if (BIT_NOT_SET(x,2))
			bytes = 5;
		else if (BIT_NOT_SET(x,1))
			bytes = 6;
		else
			return false; // Reserved

		if (src+bytes > end)
			return false; // End was malformedly truncated

		*dest_p = get_glyph_utf8(src,bytes);

		// Advance pointers. *dest has always length of single item
		dest_p++;
		src += bytes;
	}

	// Update array data length
	dest->len = dest_p - dest->buf;
	return true;
}
