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

# pragma FLIP

#include "common.h"

struct {
	const void *data;
	enum mem_type mem;
	uint16_t width;
} vars;

void init(void)
{
	if (custom_data == NULL) {
		// Use stored text in EEPROM
		vars.data = stored_glyphs();
		vars.mem = MEM_EEPROM;
	} else {
		// Use text constants
		vars.data = custom_data; //(const struct glyph_buf *)custom_data;
		vars.mem = MEM_PROG;
	}
	
	// Get string length a bit hard way. 2 is padding and 8 is char width.
	const struct glyph_buf *p = (const struct glyph_buf *)vars.data;
	vars.width = 8*(multimem_get(p->len,word,vars.mem)+2);
}

void effect(void)
{
	clear_buffer();
	// Slow down scrolling speed and wrap to beginning if needed
	int16_t pos = (ticks >> 3) % vars.width;
	scroll_text(vars.data, vars.mem, pos, render_xy);
	scroll_text(vars.data, vars.mem, pos-7, render_yz);
}

