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

#include "serial.h"
#include "serial_hex.h"

void sram_to_serial_hex(void *src, uint16_t n)
{
	uint8_t *p = (uint8_t*)src;
	for (int i=0; i<n; i++)
		send_hex_encoded(*p++);
}

void send_hex_encoded(uint8_t byte) {
    uint8_t send;
    send = num_to_hex(byte >> 4);
    serial_send(send);

    send = num_to_hex(byte & 0x0f);
    serial_send(send);
}

uint16_t serial_hex_to_sram(void *dest, uint16_t n)
{
	uint16_t i;
	uint8_t *byte_p = (uint8_t*)dest;
	for (i=0; i<n; i++) {
		read_t x = serial_read_hex_encoded();
		if (!x.good)
			break;
		*byte_p++ = x.byte;
	}
	return i;
}

read_t serial_read_hex_encoded(void) {
    read_t ret;
    uint8_t val;

    ret = serial_read_hex_char();
    val = (ret.byte << 4);
    if (!ret.good) { return ret; }

    ret = serial_read_hex_char();
    val |= ret.byte;
    ret.byte = val;

    return ret;
}

read_t serial_read_hex_char(void) {
    read_t ret = {1,0};
    ret.byte = hex_to_num(serial_read_blocking());
    if (ret.byte == NOT_HEX) {
        serial_ungetc(ret.byte);
        ret.good = 0;
    }
    return ret;
}

uint8_t hex_to_num(uint8_t c) {
	if ('0' <= c && c <= '9') {
		return c - '0';
	} else if ('a' <= c && c <= 'f') {
		return c - 'a' + 10;
	} else if ('A' <= c && c <= 'F') {
		return c - 'A' + 10;
	}
	return NOT_HEX;
}

uint8_t num_to_hex(uint8_t i) {
    if (i >= 0 && i <= 9) {
        return i + '0';
    } else if (i >= 10 && i <= 15) {
        return i + 'A' - 10;
    }

    return NOT_NUM;
}
