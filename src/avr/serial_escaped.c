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

#ifdef AVR_ELO

#include <avr/pgmspace.h>
#include <stdlib.h>
#include "serial.h"
#include "serial_escaped.h"

void send_string_from_pgm(const char * const* pgm_p)
{
	char *p = (char*)pgm_read_word_near(pgm_p);
	char c;

	// If is NULL, print is as zero-length string
	if ( p == NULL) {
		send_escaped('\0');
		return;
	}
	
	// Read byte-by-byte and write, including NUL byte
	do {
		c = pgm_read_byte_near(p++);
		send_escaped(c);
	} while (c != '\0');
}

void sram_to_serial(void *src, uint16_t n)
{
	uint8_t *p = (uint8_t*)src;
	for (int i=0; i<n; i++)
		send_escaped(*p++);
}

uint16_t serial_to_sram(void *dest, uint16_t n)
{
	uint16_t i;
	uint8_t *byte_p = (uint8_t*)dest;
	for (i=0; i<n; i++) {
		read_t x = read_escaped();
		if (!x.good)
			break;
		*byte_p++ = x.byte;
	}
	return i;
}

void send_escaped(uint8_t byte) {
	serial_send(byte);
	if (byte == ESCAPE) serial_send(LITERAL_ESCAPE);
}

read_t read_escaped() {
	read_t ret = {1,0};
	ret.byte = serial_read_blocking();

	if (ret.byte == ESCAPE) {
		ret.byte = serial_read_blocking();
		if (ret.byte != LITERAL_ESCAPE) {
			// Put bytes back and report that we got nothing.
			serial_ungetc(ret.byte);
			serial_ungetc(ESCAPE);
			ret.good = 0;
		}
	}
	return ret;
}

#endif //AVR_ELO
