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
#include <avr/sleep.h>
#include <avr/power.h>
#include "pinMacros.h"
#include "tlc5940.h"
#include "main.h"

uint8_t old_mode;

void init_ps(void)
{
	DDRD |= (1<<PD3); // PS_ON: output
	// PS_ON is low on startup
}

void cube_start(uint8_t unused)
{
	// Set sleep mode to lighter
	mode = old_mode;

	power_adc_enable();
	power_spi_enable();
	power_timer0_enable();
	power_timer1_enable();

	// Enable BLANK timer interrupt (starts SPI)
	TIMSK0 |= (1 << OCIE0A);
}

void cube_shutdown(uint8_t unused)
{
	// Set harder powersave mode
	old_mode = mode;
	mode = MODE_SLEEP;
	power_adc_disable();
	power_spi_disable();
	power_timer0_disable();
	power_timer1_disable();

	/* Pulling BLANK down reduces current consumption to
	 * 50mA. Doing that. We pull all other signals low, too. */
	pin_high(BLANK);
	pin_low(XLAT);
}
