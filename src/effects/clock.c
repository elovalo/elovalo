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

#define SECS_IN_DAY ((time_t)60*60*24)

struct {
	struct glyph_buf text;
	int32_t timezone;
} vars;

void init(void)
{
	vars.timezone = get_timezone();
}

void effect(void)
{
	vars.text.len=8;
	const struct glyph **buf = vars.text.buf;

	time_t now = time(NULL);
	time_t since_midnight = (now+vars.timezone) % SECS_IN_DAY;

	uint8_t secs = since_midnight%60;
	uint8_t minutes = since_midnight/60%60;
	uint8_t hours = since_midnight/60/60;

	buf[0] = get_glyph_ascii('0'+(hours/10));
	buf[1] = get_glyph_ascii('0'+(hours%10));
	buf[2] = get_glyph_ascii(':');
	buf[3] = get_glyph_ascii('0'+(minutes/10));
	buf[4] = get_glyph_ascii('0'+(minutes%10));
	buf[5] = get_glyph_ascii(':');
	buf[6] = get_glyph_ascii('0'+(secs/10));
	buf[7] = get_glyph_ascii('0'+(secs%10));

	clear_buffer();

	int16_t pos = ticks >> 3;

	scroll_text(&vars.text, false, pos, render_xy);
	scroll_text(&vars.text, false, pos-7, render_yz);
}
