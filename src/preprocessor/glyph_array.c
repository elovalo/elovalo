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

#include <stdio.h>
#include "../effects/lib/font8x8.h"

#define GLYPH_ARRAY_LEN 200

int main(int argc, char **argv)
{
	char *line = NULL;
	size_t buf_len = 0;

	// Allocate glyph array
	const struct glyph *glyph_buf[GLYPH_ARRAY_LEN];
	struct glyph_buf result = { GLYPH_ARRAY_LEN, glyph_buf };

	while (true) {
		// Restore buffer
		result.len = GLYPH_ARRAY_LEN;

		// Read line from user
		ssize_t len = getline(&line, &buf_len, stdin);
		if (len < 0) {
			// Most likely an EOF
			return 0;
		}

		// Convert to glyphs. Omit \0 in the end
		bool ret = utf8_string_to_glyphs(line, len-1, &result);
		if (!ret) {
			printf("UTF-8 decoding error\n");
			continue;
		}
		
		// Convert glyphs to C array
		printf("{%d,{",result.len);
		for (int i=0; i<result.len; i++) {
			if (i)
				putchar(',');
			const struct glyph *g = result.buf[i];
			int pos = g-glyphs; // calculate correct memory position
			printf("glyphs+%d",pos);
		}
		printf("}}\n");
	}
	// Does never end
}
