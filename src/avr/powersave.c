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

#include <stdint.h>
#include <avr/io.h>
#include "pinMacros.h"
#include "tlc5940.h"

void init_ps(void)
{
	DDRD |= (1<<PD3); // PS_ON: output
	// PS_ON is low on startup
}

void cube_start(uint8_t unused)
{
	// Enable BLANK timer interrupt (starts SPI)
	TIMSK0 |= (1 << OCIE0A);
}

void cube_shutdown(uint8_t unused)
{
	// Disable BLANK timer interrupt (stops SPI)
	TIMSK0 &= ~(1 << OCIE0A);

	/* Pulling BLANK down reduces current consumption to
	 * 50mA. Doing that. We pull all other signals low, too. */
	pin_high(BLANK);
	pin_low(XLAT);
}
