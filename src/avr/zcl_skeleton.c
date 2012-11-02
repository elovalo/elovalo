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

#include <util/crc16.h>
#include <avr/pgmspace.h>
#include <stdlib.h>

#include "serial.h"
#include "../common/pgmspace.h"
#include "main.h"

// Lengths
#define ZCL_MESSAGE_HEADER_LEN 11
#define READ_RESP_HEADER_LEN 4
#define MAC_LEN 8

// Frame types
#define ACK 'K' // Successfully received last packet
#define NAK 'N' // Error, please resend last packet
#define STX 'S' // Sending a new packet

#define PACKET_BEGIN '0' // Every packet starts with this

// Payload Channels
#define ZCL_CHANNEL 0x01 // ZCL message channel

// Elovalo end point id
#define PROFILE 1024
#define EP_ID 70

// Cluster IDs
#define CLUSTERID_BASIC 0x00
#define CLUSTERID_ELOVALO 0x500

// Command IDs
#define CMDID_READ 0x00
#define CMDID_READ_RESPONSE 0x01
#define CMDID_WRITE 0x02
#define CMDID_WRITE_RESPONSE 0x04

// Attributes
// Basic cluster
#define ATTR_DEVICE_ENABLED 0x0012
#define ATTR_ALARM_MASK 0x0013

// Elovalo cluster
#define ATTR_IEEE_ADDRESS 0x401
#define ATTR_OPERATING_MODE 0x01
#define ATTR_EFFECT_TEXT 0x02
#define ATTR_PLAYLIST 0x03
#define ATTR_TIMEZONE 0x04
#define ATTR_TIME 0x05
#define ATTR_EFFECT_NAMES 0x06
#define ATTR_PLAYLIST_NAMES 0x07
#define ATTR_PLAYLIST_EFFECTS 0x08
#define ATTR_EFFECT 0x09
#define ATTR_HW_VERSION 0x10
#define ATTR_SW_VERSION 0x11

// Data types
#define TYPE_BOOLEAN 0x10
#define TYPE_UINT8 0x20
#define TYPE_INT32 0x2b
#define TYPE_ENUM 0x30
#define TYPE_OCTET_STRING 0x41
#define TYPE_LONG_OCTET_STRING 0x43
#define TYPE_UTC_TIME 0xe2
#define TYPE_IEEE_ADDRESS 0xf0

// Data type lengths
#define TYPELEN_BOOLEAN 1
#define TYPELEN_UINT8 1
#define TYPELEN_INT32 4
#define TYPELEN_ENUM 1
#define TYPELEN_UTC_TIME 4
#define TYPELEN_IEEE_ADDRESS 8

// Status IDs
#define STATUS_SUCCESS 0x00
#define STATUS_FAILURE 0x01
#define STATUS_UNSUP_CLUSTER_COMMAND 0x80
#define STATUS_UNSUPPORTED_ATTRIBUTE 0x86
#define STATUS_INVALID_VALUE 0x87
#define STATUS_READ_ONLY 0x88

// Values
#define BOOL_TRUE 0x01
#define BOOL_FALSE 0x00

// Serial port
#define NOT_HEX 0xff
#define NOT_NUM 0x00

// Parser states
#define PARSER_STATE_DEFAULT 0x00
#define PARSER_STATE_INCORRECT_MAC 0x01

// Read buffer defines
#define READ_BUF_CAPACITY 32
#define READ_BUF_OK 0x00
#define READ_BUF_OVERFLOW 0x01

// Data reading functions
typedef uint8_t (*reader_t)(void);

typedef union frame_control {
	struct {
		unsigned type: 2;
		unsigned manu_specific: 1;
		unsigned direction: 1;
		unsigned disable_def_resp: 1;
		unsigned reserved: 3;
	};

	uint8_t integer;
} frame_control_t;

typedef union hex_val {
	struct {
		unsigned one: 8;
		unsigned two: 8;
	};

	uint16_t integer;
} hex_value_t;

static void send_error(void);
static void send_ok(void);
static uint8_t read_packet(void);
static uint8_t process_payload(uint16_t length);
static uint8_t process_cmd_frame(uint16_t cluster, uint16_t length);
static void process_read_cmd(uint16_t cluster, uint16_t length);
static void read_payload_crc(uint16_t length);
static void write_attr_resp_header(uint16_t attr, uint8_t type);
static uint16_t read_cmd_length(uint16_t cluster, uint16_t msg_len);
static void write_attr_resp_fail(void);

static void write_packet_header(uint16_t length);
static void write_payload_header(void);
static void write_zcl_header(uint8_t cmd);
static void write_effect_names(void);
static void process_write_cmd(uint8_t cluster, uint16_t length);
static void write_default_response(uint8_t cmd, uint8_t status);
static void write_unsupported_read_attribute(uint16_t attr);

static void reset_read_crc(void);
static void reset_write_crc(void);

static void write_hex_16(uint16_t);
static void write_hex_byte(uint8_t);
static void write_hex_crc(uint8_t);
static void write_hex_crc_16(uint16_t);
static void write_pgm_string_hex_crc(const char * const* pgm_p);

static uint8_t ser_to_rbuf_read(void);
static uint8_t read_rbuf(void);
static void reset_rbuf_i(void);

static uint8_t accept(reader_t, uint8_t);
static uint8_t read_hex_byte(reader_t);
static uint8_t read_hex_crc_byte(reader_t);
static uint8_t read_hex(reader_t);
static uint8_t read_hex_crc(reader_t);
static uint16_t read_hex_16(reader_t);
static uint16_t read_hex_crc_16(reader_t);

static uint8_t htoi(uint8_t);
static hex_value_t itohval(uint8_t);
static uint8_t itoh(uint8_t);

uint8_t parser_state = PARSER_STATE_DEFAULT;
uint8_t error_read = 0;

uint8_t rbuf[READ_BUF_CAPACITY];
uint8_t rbuf_i = 0;
uint8_t rbuf_state = READ_BUF_OK;

uint16_t write_crc = 0xffff;
uint16_t read_crc = 0xffff;

// ZCL Header variables
uint8_t transaction_seq = 0;
uint8_t mac[] = {0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef};

// ATI
#define ATI 'A'
#define ATI_LEN 36
uint8_t ati_resp[] = "C2IS,elovalo,v1.5,01:23:45:67:89:AB\n";

// REFACTORING TODO ser_read and friends to upper-case
#define ser_read (&serial_read_blocking)
#define ser_rbuf_read (&ser_to_rbuf_read)
#define rbuf_read (&read_rbuf)

void process_zcl_frame(uint8_t frametype) {
	parser_state = PARSER_STATE_DEFAULT;

	switch (frametype) {
	case ACK:
		//TODO: timeout error if not received 1s after sending a message
		break;
	case NAK:
		//TODO: resend last packet
		break;
	case STX:
	{
		uint8_t err = read_packet();
		if (err) {
			send_error();
		} else {
			send_ok();
		}
		break;
	}
	case ATI:
	{
		uint8_t b;
		b = serial_read_blocking();
		if (b != 'T') { break; }
		b = serial_read_blocking();
		if (b != 'I') { break; }
		for (uint8_t i = 0; i < ATI_LEN; i++) {
			serial_send(ati_resp[i]);
		}
	}
	default:
		//TODO: handle unknown content
		break;
	}
}

static void send_error(void) {
	serial_send(NAK);
}

static void send_ok(void) {
	serial_send(ACK);
}

static uint8_t read_packet(void) {
	error_read = 0;
	reset_read_crc();

	uint8_t begin;
	begin = serial_read_blocking();
	if (begin != PACKET_BEGIN) {
		// Do nothing or return error?
		return 1;
	}
	uint16_t length = read_hex_16(ser_read);
	
	uint8_t err = process_payload(length);
	if (err) { return 1; }

	uint16_t msg_crc = read_hex_16(ser_read);
	if (msg_crc != read_crc || error_read) {
		return 1;
	}

	return 0;
}

static uint8_t process_payload(uint16_t length) {
	// Confirm message channel
	if (!accept(ser_read, ZCL_CHANNEL)) {
		return 1;
	}

	// Confirming MAC address
	for (uint8_t i = 0; i < MAC_LEN; i++) {
		if (!accept(ser_read, mac[i])) {
			parser_state = PARSER_STATE_INCORRECT_MAC;
		}
	}

	// Confirming the end point
	if (accept(ser_read, EP_ID)) {
		return 1;
	}

	uint16_t profile = read_hex_crc_16(ser_read);
	uint16_t cluster = read_hex_crc_16(ser_read);

	length -= ZCL_MESSAGE_HEADER_LEN;

	if (parser_state == PARSER_STATE_INCORRECT_MAC) {
		read_payload_crc(length);
		return 0;
	}

	return process_cmd_frame(cluster, length);
}

static uint8_t process_cmd_frame(uint16_t cluster, uint16_t length) {
	frame_control_t frame_control;
	frame_control.integer = read_hex_crc(ser_read);

	if (frame_control.manu_specific) {
		uint16_t manu_spec = read_hex_crc_16(ser_read);
		length -= 2;
	}
	uint8_t trans_seq = read_hex_crc(ser_read);
	uint8_t cmd = read_hex_crc(ser_read);
	length -= 3;

	switch (cmd) {
		case CMDID_READ:
			process_read_cmd(cluster, length);
			break;
		case CMDID_WRITE:
			process_write_cmd(cluster, length);
			break;
		default:
			return 1;
	}
	return 0;
}

static void process_read_cmd(uint16_t cluster, uint16_t len) {
	uint16_t resp_len = read_cmd_length(cluster, len);
	uint16_t attr;

	reset_write_crc();
	reset_rbuf_i();

	write_packet_header(resp_len);
	write_payload_header();
	write_zcl_header(CMDID_READ_RESPONSE);
	
	for (uint16_t i = 0; i < len; i += 2) {
		attr = read_hex_crc_16(rbuf_read);
		if (cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				write_attr_resp_header(ATTR_DEVICE_ENABLED, TYPE_BOOLEAN);
				write_hex_crc(get_mode());
				break;
			case ATTR_ALARM_MASK:
				write_attr_resp_header(ATTR_ALARM_MASK, TYPE_BOOLEAN);
				//write_hex_crc(ALARM_MASK); //TODO
				break;
			case ATTR_IEEE_ADDRESS:
				write_attr_resp_header(ATTR_IEEE_ADDRESS, TYPE_IEEE_ADDRESS);
				for (uint8_t i = 0; i < MAC_LEN; i++) {
					write_hex_crc(mac[i]);
				}
				break;
			default:
				write_unsupported_read_attribute(attr);
				break;
			}
		} else if (cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_OPERATING_MODE:
			{
				write_attr_resp_header(ATTR_OPERATING_MODE, TYPE_ENUM);
				uint8_t mode = get_mode();
				if (mode == MODE_SLEEP || mode == MODE_IDLE) {
					write_hex_crc(0);
				} else if (mode == MODE_EFFECT) {
					write_hex_crc(1);
				} else if (mode == MODE_PLAYLIST) {
					write_hex_crc(2);
				}
				break;
			}
			/*case ATTR_EFFECT_TEXT:
				write_attr_resp_header(ATTR_EFFECT_TEXT, TYPE_OCTET_STRING);
				write_effect_text(); //TODO
				break;
			case ATTR_PLAYLIST:
				write_attr_resp_header(ATTR_PLAYLIST, TYPE_UINT8);
				write_hex_crc(current_playlist);
				break;
			case ATTR_TIMEZONE:
				write_attr_resp_header(ATTR_TIMEZONE, TYPE_INT32);
				write_timezone(); //TODO
				break;
			case ATTR_TIMEZONE:
				write_attr_resp_header(ATTR_TIME, TYPE_UTC_TIME);
				write_time(); //TODO
				break;
			case ATTR_EFFECT_NAMES:
				write_attr_resp_header(ATTR_EFFECT_NAMES, TYPE_LONG_OCTET_STRING);
				write_effect_names();
				break;
			case ATTR_PLAYLIST_NAMES:
				write_attr_resp_header(ATTR_PLAYLIST_NAMES, TYPE_LONG_OCTET_STRING);
				write_playlist_names(); //TODO
				break;
			case ATTR_PLAYLIST_EFFECTS:
				write_attr_resp_header(ATTR_PLAYLIST_EFFECTS, TYPE_OCTET_STRING);
				write_playlist_effects(); //TODO
				break;
			case ATTR_EFFECT:
				write_attr_resp_header(ATTR_EFFECT, TYPE_UINT8);
				write_hex_crc(current_effect);
				break;
			case ATTR_HW_VERSION:
				write_attr_resp_header(ATTR_HW_VERSION, TYPE_OCTET_STRING);
				write_hw_version(); //TODO
				break;
			case ATTR_SW_VERSION:
				write_attr_resp_header(ATTR_SW_VERSION, TYPE_OCTET_STRING);
				write_sw_version(); //TODO
				break;*/
			default:
				write_unsupported_read_attribute(attr);
				break;
			}
		}
	}
	
	write_hex_16(write_crc);
}

/**
 * Used when the MAC address is incorrect to make sure that the CRC
 * check works correctly
 */
static void read_payload_crc(uint16_t length) {
	for (uint16_t i = 0; i < length; i++) {
		read_hex_crc(ser_read);
	}
}

static void write_attr_resp_header(uint16_t attr, uint8_t type) {
	write_hex_crc_16(attr);
	write_hex_crc(STATUS_SUCCESS);
	write_hex_crc(type);
}

static void write_unsupported_read_attribute(uint16_t attr) {
	write_hex_crc_16(attr);
	write_hex_crc(STATUS_UNSUPPORTED_ATTRIBUTE);
}

static uint16_t read_cmd_length(uint16_t cluster, uint16_t msg_len) {
	uint16_t length = 1; // Because payload contains the channel byte

	reset_rbuf_i();
	for (uint16_t i = 0; i < msg_len; i += 2) {
		uint16_t attr = read_hex_crc_16(ser_to_rbuf_read);
		length += READ_RESP_HEADER_LEN;

		if (cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				length += TYPELEN_BOOLEAN;
				break;
			case ATTR_ALARM_MASK:
				length += TYPELEN_UINT8;
				break;
			case ATTR_IEEE_ADDRESS:
				length += TYPELEN_IEEE_ADDRESS;
				break;
			default:
				break;
			}
		} else if (cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_OPERATING_MODE:
				length += TYPELEN_ENUM;
				break;
			/*case ATTR_EFFECT_TEXT:
				length += //TODO;
				break;
			case ATTR_PLAYLIST:
				length += //TODO;
				break;
			case ATTR_TIMEZONE:
				length += TYPELEN_INT32;
				break;
			case ATTR_TIME:
				length += TYPELEN_UTC_TIME;
				break;
			case ATTR_EFFECT_NAMES:
				length += //TODO;
				break;
			case ATTR_PLAYLIST_NAMES:
				length += //TODO;
				break;
			case ATTR_PLAYLIST_EFFECTS:
				length += //TODO;
				break;
			case ATTR_EFFECT:
				length += TYPELEN_UINT8;
				break;
			case ATTR_HW_VERSION:
				length += //TODO;
				break;
			case ATTR_SW_VERSION:
				length += //TODO;
				break;
			default:
				length += ;
				break;*/
			}
		}
	}

	return length;
}

static void write_packet_header(uint16_t length) {
	reset_write_crc();
	serial_send(STX);
	write_hex_crc(PACKET_BEGIN);
	write_hex_16(length);
}

static void write_payload_header(void) {
	write_hex_crc(ZCL_CHANNEL);
	// write MAC
	for (uint8_t i = 0; i < MAC_LEN; i++) {
		write_hex_crc(mac[i]);
	}

	write_hex_crc(EP_ID);
	write_hex_crc_16(PROFILE);
	write_hex_crc_16(CLUSTERID_ELOVALO);
}

static void write_zcl_header(uint8_t cmd){
	frame_control_t fc;
	fc.integer = 0;

	write_hex_crc(fc.integer);
	write_hex_crc(transaction_seq++);
	if (transaction_seq == 0xff) {
		transaction_seq = 0;
	}
	write_hex_crc(cmd);
}

static void write_effect_names(void) {
	write_hex_crc('[');
	for (uint8_t i = 0; i < effects_len; i++) {
		write_hex_crc('"');
		write_pgm_string_hex_crc(&effects[i].name);
		write_hex_crc('"');
		if (i < effects_len - 1) {
			write_hex_crc(',');
		};
	}
	write_hex_crc(']');
}

static void process_write_cmd(uint8_t cluster, uint16_t length) {
	for (uint16_t i = 0; i < length; i++) {
		uint16_t attr = read_hex_crc_16(ser_read);
		if (cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				if (accept(ser_read, TYPE_BOOLEAN)) {
					uint8_t state = read_hex_crc(ser_read);
					if (state == BOOL_TRUE) {
						set_mode(MODE_PLAYLIST);
					} else if (state == BOOL_FALSE) {
						set_mode(MODE_IDLE);
					}
				}
				break;
			case ATTR_ALARM_MASK:
				//TODO
				break;
			case ATTR_IEEE_ADDRESS:
				if (accept(ser_read, TYPE_IEEE_ADDRESS)) {
					for (uint8_t i = 0; i < MAC_LEN; i++) {
						mac[i] = read_hex_crc(ser_read);
					}
				}
				break;
			}
		} else if (cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_OPERATING_MODE:
				if (accept(ser_read, TYPE_ENUM)) {
					uint8_t mode = read_hex_crc(ser_read);
					set_mode(mode);
				}
				break;
			case ATTR_EFFECT_TEXT:
				if (accept(ser_read, TYPE_OCTET_STRING)) {
					/*uint8_t slen = read_hex_crc();
					for (uint8_t i = 0; i < slen; i++) {
						effect_text[i] = read_hex_crc(); // TODO
					}*/
				}
				break;
			case ATTR_PLAYLIST:
				if (accept(ser_read, TYPE_UINT8)) {
					//current_playlist(read_hex_crc());
				}
				break;
			case ATTR_TIMEZONE:
				if (accept(ser_read, TYPE_INT32)) {
					//TODO			}
				}
				break;
			case ATTR_TIME:
				if (accept(ser_read, TYPE_UTC_TIME)) {
					//TODO
				}
				break;
			case ATTR_EFFECT:
				if (accept(ser_read, TYPE_UINT8)) {
					//uint8_t current_effect = read_hex_crc(); //TODO
				}
				break;
			}
		}
	}
	//write_success_write_resp();
}

static void write_default_response(uint8_t cmd, uint8_t status) {
	write_zcl_header(2);
	write_hex_crc(cmd);
	write_hex_crc(status);
}

static void write_attr_resp_fail(void) {
}

//------ Serial port functions ---------

// CRC
static void reset_read_crc(void) {
	read_crc = 0xffff;
}

static void reset_write_crc(void) {
	write_crc = 0xffff;
}

// Writes

static void write_hex_crc(uint8_t byte) {
	write_crc = _crc_ccitt_update(write_crc, byte);
	hex_value_t val;
	val = itohval(byte);

	serial_send(val.one);
	serial_send(val.two);
}

static void write_hex_crc_16(uint16_t data) {
	write_hex_crc(data >> 8);
	write_hex_crc(data & 0x00ff);
}

static void write_hex_16(uint16_t data) {
	hex_value_t val;

	val = itohval(data >> 8);
	serial_send(val.one);
	serial_send(val.two);

	val = itohval(data & 0x00ff);
	serial_send(val.one);
	serial_send(val.two);
}

static void write_hex_byte(uint8_t byte) {
	uint8_t val = itoh(byte);
	serial_send(val);
}

static void write_pgm_string_hex_crc(const char * const* pgm_p) {
	char *p = (char*)pgm_read_word_near(pgm_p);
	char c;

	// If is NULL, print is as zero-length string
	if ( p == NULL) {
		write_hex_crc(' ');
		return;
	}
	
	// Read byte-by-byte and write, not including NUL byte
	c = pgm_read_byte_near(p++);
	while (c != '\0') {
		write_hex_crc(c);
		c = pgm_read_byte_near(p++);
	}
}

// Reads

/**
 * A reader_t reading from serial port to the
 * read-command buffer. Returns the value read.
 */
static uint8_t ser_to_rbuf_read(void) {
	if (rbuf_i >= READ_BUF_CAPACITY) {
		rbuf_state = READ_BUF_OVERFLOW;
		return 0;
	}
	rbuf[rbuf_i++] = serial_read_blocking();
	return rbuf[rbuf_i];
}

/**
 * A reader_t reading a byte from the read-command buffer
 */
static uint8_t read_rbuf(void) {
	if (rbuf_i >= READ_BUF_CAPACITY) {
		rbuf_state = READ_BUF_OVERFLOW;
		return 0;
	}
	return rbuf[rbuf_i++];
}

/**
 * Resets the read-command buffer reading index
 */
static void reset_rbuf_i(void) {
	rbuf_i = 0;
}

/**
 * Reads a single byte, compares it to the given parameter
 * and returns if it matches.
 */
static uint8_t accept(reader_t r, uint8_t val) {
	uint8_t b = read_hex_crc(r);

	if (b == val) {
		return 1;
	}
	return 0;
}

/**
 * Read a single byte and hex decodes it
 */
static uint8_t read_hex_byte(reader_t r) {
	uint8_t read;
	read = r();
	return htoi(read);
}

/**
 * Read a single byte and hex decode it while updating the CRC
 */
static uint8_t read_hex_crc_byte(reader_t r) {
	uint8_t read;
	read = r();
	read_crc = _crc_ccitt_update(read_crc, read);
	return htoi(read);
}

/**
 * Read two bytes and hex decode them to a single value
 */
static uint8_t read_hex(reader_t r) {
	uint8_t read;
	uint8_t val;

	read = r();
	val = (itoh(read) << 4);

	read = r();
	val |= itoh(read);

	return val;
}

/**
 * Read two bytes and hex decode them to a single value while
 * updating the CRC value
 */
static uint8_t read_hex_crc(reader_t r) {
	uint8_t read;
	uint8_t val;

	read = r();
	read_crc = _crc_ccitt_update(read_crc, read);
	val = (itoh(read) << 4);

	read = r();
	read_crc = _crc_ccitt_update(read_crc, read);
	val |= itoh(read);

	return val;
}

/**
 * Read four bytes and hex decode them to a single value.
 */
static uint16_t read_hex_16(reader_t r) {
	uint16_t ret;
	uint8_t val;
	val = read_hex(r);

	ret = (val << 8);

	val = read_hex(r);
	ret |= val;

	return ret;
}

/**
 * Read four bytes and hex decode them to a single value
 * while updating the CRC.
 */
static uint16_t read_hex_crc_16(reader_t r) {
	uint16_t ret;
	uint8_t val;
	val = read_hex_crc(r);

	ret = (val << 8);

	val = read_hex_crc(r);
	ret |= val;

	return ret;
}

// --------- Hex conversions --------

/**
 * Convert a hexadecimal character to integer.
 */
uint8_t htoi(uint8_t c) {
	if ('0' <= c && c <= '9') {
		return c - '0';
	} else if ('a' <= c && c <= 'f') {
		return c - 'a' + 10;
	} else if ('A' <= c && c <= 'F') {
		return c - 'A' + 10;
	}
	return NOT_HEX;
}

/**
 * Convert an integer to hex characters.
 */
hex_value_t itohval(uint8_t i) {
	hex_value_t val;
	val.one = itoh(i >> 4);
	val.two = itoh(i & 0x0f);
	return val;
}

/**
 * Convert an integer to a hexadecimal character.
 */
uint8_t itoh(uint8_t i) {
	if (i >= 0 && i <= 9) {
		return i + '0';
	} else if (i >= 10 && i <= 15) {
		return i + 'A' - 10;
	}

	return NOT_NUM;
}
