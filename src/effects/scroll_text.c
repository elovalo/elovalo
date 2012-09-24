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

PROGMEM const char default_text[] = "\x05""ERROR";

void effect(void)
{
	// TODO: Should be asserted instead of just sending "ERROR"
	const char *text = custom_data == NULL? default_text: (const char*)custom_data;

	clear_buffer();

	int16_t pos = ticks >> 3;
	scroll_text(text, true, pos, cd_render_xy);
	scroll_text(text, true, pos-7, cd_render_yz);
}

