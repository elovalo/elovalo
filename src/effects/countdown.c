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
# pragma MAX_FPS 1

#include "common.h"

struct {
	uint8_t cur;
	struct glyph_buf text;
} vars;

void init(void) {
	vars.cur = 15;
}

void effect(void)
{
	vars.text.len = 2;
	vars.text.buf[0] = get_glyph_ascii('0' + (vars.cur / 10));
	vars.text.buf[1] = get_glyph_ascii('0' + (vars.cur % 10));

	clear_buffer();

	scroll_text(&vars.text, MEM_SRAM, 9, render_yz);
	scroll_text(&vars.text, MEM_SRAM, 16, render_xy);

	if(vars.cur > 0) vars.cur--;
}
