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

//8 bit read value
typedef struct {
	uint8_t good;
	uint8_t byte;
} read_t;

//16 bit read value
typedef struct {
	uint8_t good;
	uint16_t val;
} read16_t;

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

/**
 * Reads and hex decodes four bytes to a uint16_t value and updates the internal CRC value
 */
read16_t serial_read_hex_uint16_crc(void);

/**
 * Reads and hex decodes four bytes to a uint16_t value
 */
read16_t serial_read_hex_uint16(void);

/**
 * Resets the internal CRC value.
 * Use this function before beginning CRC reads
 */
void reset_crc(void);

/**
 * Returns the currently calculated CRC value
 */
uint16_t get_crc(void);

/**
 * Reads and hex decodes two bytes to a single value from the serial port. Also updates internal CRC value (call reset_crc before starting calls to this function).
 */
read_t serial_read_hex_encoded_crc(void);

/**
 * Reads and hex decodes a single byte from the serial port. Also updates internal CRC value (call reset_crc before starting calls to this function).
 */
read_t serial_read_hex_byte_crc(void);

/**
 * Reads and hex decodes two bytes to a single value from the serial port.
 */
read_t serial_read_hex_encoded(void);

/**
 * Reads and hex decodes a single byte from the serial port.
 */
read_t serial_read_hex_byte(void);

/**
 * Converts an ASCII hex char to a corresponding number value.
 * Returns NOT_HEX if incorrect value given.
 */
uint8_t hex_to_num(uint8_t c);

/**
 * Converts a number to a single hex ASCII char.
 * Returns NOT_NUM if given value is not between 0-15.
 */
uint8_t num_to_hex(uint8_t i);
