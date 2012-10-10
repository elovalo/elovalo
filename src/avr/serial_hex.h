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

#define NOT_HEX 254
#define NOT_NUM 0

typedef struct {
	uint8_t good;
	uint8_t byte;
} read_t;

/**
 * Send n bytes of hex encoded data starting from
 * SRAM pointer src to serial port
 */
void sram_to_serial_hex(void *src, uint16_t n);

/**
 * Sends value to the serial port encoded as two hex ASCII bytes
 */
void send_hex_encoded(uint8_t byte);

/**
 * Reads and hex decodes n bytes of data from serial port to given SRAM
 * location. If data is encoded incorrectly, stop reading. Function returns
 * the number of bytes read. If reading was interrupted due to incorrect
 * encoding return value may be less than n.
 */
uint16_t serial_hex_to_sram(void *dest, uint16_t n);

/*
 * Reads and hex decodes two bytes to a single value from the serial port.
 */
read_t serial_read_hex_encoded(void);

/*
 * Reads and hex decodes a single byte from the serial port.
 */
read_t serial_read_hex_char(void);

/*
 * Converts an ASCII hex char to a corresponding number value.
 * Returns NOT_HEX if incorrect value given.
 */
uint8_t hex_to_num(uint8_t);

/*
 * Converts a number to a single hex ASCII char.
 * Returns NOT_NUM if given value is not between 0-15.
 */
uint8_t num_to_hex(uint8_t);
