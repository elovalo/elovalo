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
#define TIMEZONE_SECS (3*60*60)

void effect(void)
{
	char text[9];
	text[0] = 8;
	text[3] = ':';
	text[6] = ':';

	time_t now = time(NULL);
	time_t since_midnight = (now+TIMEZONE_SECS) % SECS_IN_DAY;

	uint8_t secs = since_midnight%60;
	uint8_t minutes = since_midnight/60%60;
	uint8_t hours = since_midnight/60/60;

	text[1] = '0'+(hours/10);
	text[2] = '0'+(hours%10);
	text[4] = '0'+(minutes/10);
	text[5] = '0'+(minutes%10);
	text[7] = '0'+(secs/10);
	text[8] = '0'+(secs%10);

	clear_buffer();

	int16_t pos = ticks >> 3;

	scroll_text(text, pos, render_xy);
	scroll_text(text, pos-7, render_yz);
}
