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

#ifdef AVR_ZCL

#include <util/crc16.h>
#include <avr/pgmspace.h>
#include <stdlib.h>
#include <stdbool.h>

#include "serial.h"
#include "../common/pgmspace.h"
#include "main.h"
#include "serial_zcl.h"

// Sharing tlc5940 gs_buf_back to conserve memory
#include "../common/cube.h" 
#include "tlc5940.h"

// Lengths
#define PACKET_HEADER_LEN 17
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
#define ENDPOINT 70

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
#define NOT_HEX 'G'
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
typedef void (*writer_t)(uint8_t);

typedef union hex_val {
	struct {
		unsigned low: 8;
		unsigned high: 8;
	};
} hex_value_t;

enum zcl_status {
	ZCL_SUCCESS,
	ZCL_BAD_PROFILE,
	ZCL_BAD_ENDPOINT,
	ZCL_BAD_COMMAND,
	ZCL_IMPOSSIBLE
};

static bool read_packet(void);
static void process_payload();
static enum zcl_status process_cmd_frame();
static enum zcl_status process_read_cmd();
static void write_attr_resp_header(uint16_t attr, uint8_t type);
static uint16_t resp_read_len(void);
static void write_attr_resp_fail(void);

static void write_packet_header(uint16_t length);
static void write_payload_header(void);
static void write_zcl_header(uint8_t cmd);
static void write_effect_names(void);
static enum zcl_status process_write_cmd(void);
static void write_default_response(uint8_t cmd, uint8_t status);
static void write_unsupported_attribute(uint16_t attr);

static uint8_t msg_next(void);
static bool msg_available(void);

static inline void write(writer_t, uint8_t);
static void write_16(writer_t, uint16_t);
static void write_64(writer_t, uint64_t);
static void write_pgm_string(writer_t w, const char * const* pgm_p);

static void serial_send_hex_crc(uint8_t);
static void reset_write_crc(void);
static inline void serial_send_hex(uint8_t);

static bool accept(reader_t, uint8_t);
static uint8_t read(reader_t);
static uint16_t read_16(reader_t);
static uint64_t read_64(reader_t);

static hex_value_t itohval(uint8_t);
static uint8_t itoh(uint8_t i);

uint16_t write_crc = 0xffff;
uint16_t msg_i = 0; // Packet message read index

// ZCL Header variables
uint8_t transaction_seq = 0;
uint64_t mac = 0xefcdab8967452301;

// ATI
#define ATI 'A'
uint8_t ati_resp[] = "C2IS,elovalo,v1.5,01:23:45:67:89:AB:CD:EF\n";

// reader_t and writer_t functions
#define MSG_R (&msg_next)
#define HEX_CRC_W (&serial_send_hex_crc)
#define HEX_W (&serial_send_hex)

#define SERIAL_BUF_LEN GS_BUF_BYTES
#define serial_buf gs_buf_back
#define packet ((struct packet_s *)(gs_buf_back))

void process_zcl_frame(uint8_t frametype) {
	switch (frametype) {
	case ACK:
		//TODO: timeout error if not received 1s after sending a message
		break;
	case NAK:
		//TODO: resend last packet
		break;
	case STX:
	{
		// Wait for flip to be sure back buffer stays intact
		while (flags.may_flip);

		// Reading and verifying serial buffer contents
		if (!read_packet()) {
			serial_send(NAK);
			break;
		}

		// Message was transmitted correctly
		serial_send(ACK);
		process_payload();

		break;
	}
	case ATI:
	{
		if (serial_read_blocking() != 'T') break;
		if (serial_read_blocking() != 'I') break;
		for (uint8_t i = 0; i < sizeof(ati_resp)-1; i++) {
			serial_send(ati_resp[i]);
		}
	}
	default:
		//TODO: handle unknown content
		break;
	}
}

static bool read_packet(void) {
	reset_write_crc();

	// Read "reserved" byte
	if (serial_read_blocking() != PACKET_BEGIN) {
		// Do nothing or return error?
		return false;
	}

	// FIXME check if packet length is not longer than SERIAL_BUF_LEN

	uint16_t crc = 0xffff;

	for (uint16_t i = 0; i < packet->length; i++) {
		crc = _crc_ccitt_update(crc, serial_buf[i+2]);
	}
	uint16_t msg_crc;
	msg_crc = serial_buf[packet->length + 2] << 8;
	msg_crc |= serial_buf[packet->length + 3];


	return msg_crc == crc;
}

static void process_payload(void) {

	// Only ZCL messages are supported.
	if (packet->channel != ZCL_CHANNEL) return;

	// Filter out messages that do not belong to me
	if (packet->mac != mac) return;

	/* If using manufacturer specific extensions, do not
	 * touch. The payload is distorted anyway */
	if (packet->mfr_specific) return;

	/* Filter out profiles and endpoints that are not supported on
	 * this device. FIXME: Generate error responses for these. */
	enum zcl_status status;
	if (packet->endpoint != ENDPOINT) {
		status = ZCL_BAD_ENDPOINT;
	} else if (packet->profile != PROFILE) {
		status = ZCL_BAD_PROFILE;
	} else {
		status = process_cmd_frame();
	}

	// FIXME error handling
}

static enum zcl_status process_cmd_frame(void) {
	switch (packet->cmd_type) {
	case CMDID_READ:
		return process_read_cmd();
	case CMDID_WRITE:
		return process_write_cmd();
	default:
		return ZCL_BAD_COMMAND;
	}

	return ZCL_IMPOSSIBLE;
}

static enum zcl_status process_read_cmd() {
	uint16_t resp_len = resp_read_len();

	write_packet_header(resp_len);
	write_payload_header();
	write_zcl_header(CMDID_READ_RESPONSE);

	while(msg_available()) {
		uint16_t attr;
		attr = read_16(MSG_R);
		
		if (packet->cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				write_attr_resp_header(ATTR_DEVICE_ENABLED, TYPE_BOOLEAN);
				write(HEX_CRC_W, get_mode());
				break;
			case ATTR_ALARM_MASK:
				write_attr_resp_header(ATTR_ALARM_MASK, TYPE_BOOLEAN);
				//write(HEX_CRC_W, ALARM_MASK); //TODO
				break;
			case ATTR_IEEE_ADDRESS:
			{
				write_attr_resp_header(ATTR_IEEE_ADDRESS, TYPE_IEEE_ADDRESS);
				write_64(HEX_CRC_W, mac);
				break;
			}
			default:
				write_unsupported_attribute(attr);
				break;
			}
		} else if (packet->cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_OPERATING_MODE:
			{
				write_attr_resp_header(ATTR_OPERATING_MODE, TYPE_ENUM);
				uint8_t mode = get_mode();
				if (mode == MODE_SLEEP || mode == MODE_IDLE) {
					write(HEX_CRC_W, 0);
				} else if (mode == MODE_EFFECT) {
					write(HEX_CRC_W, 1);
				} else if (mode == MODE_PLAYLIST) {
					write(HEX_CRC_W, 2);
				}
				break;
			}
			/*case ATTR_EFFECT_TEXT:
				write_attr_resp_header(ATTR_EFFECT_TEXT, TYPE_OCTET_STRING);
				write_effect_text(); //TODO
				break;
			case ATTR_PLAYLIST:
				write_attr_resp_header(ATTR_PLAYLIST, TYPE_UINT8);
				write(HEX_CRC_W, current_playlist);
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
				write(HEX_CRC_W, current_effect);
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
				write_unsupported_attribute(attr);
				break;
			}
		}

	}

	write_16(HEX_W, write_crc);

	return ZCL_SUCCESS;
}

static void write_attr_resp_header(uint16_t attr, uint8_t type) {
	write_16(HEX_CRC_W, attr);
	write(HEX_CRC_W, STATUS_SUCCESS);
	write(HEX_CRC_W, type);
}

static void write_unsupported_attribute(uint16_t attr) {
	write_16(HEX_CRC_W, attr);
	write(HEX_CRC_W, STATUS_UNSUPPORTED_ATTRIBUTE);
}

static uint16_t resp_read_len(void) {
	uint16_t length = 1; // Because payload contains the channel byte
	uint16_t attr;

	while(msg_available()) {
		attr = read_16(MSG_R);
		length += READ_RESP_HEADER_LEN;

		if (packet->cluster == CLUSTERID_BASIC) {
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
		} else if (packet->cluster == CLUSTERID_ELOVALO) {
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
	serial_send(STX);
	write(HEX_CRC_W, PACKET_BEGIN);
	write_16(HEX_W, length);
}

static void write_payload_header(void) {
	write(HEX_CRC_W, ZCL_CHANNEL);
	write_64(HEX_CRC_W, mac);

	write(HEX_CRC_W, ENDPOINT);
	write_16(HEX_CRC_W, PROFILE);
	write_16(HEX_CRC_W, CLUSTERID_ELOVALO);
}

static void write_zcl_header(uint8_t cmd){
	// Send out the frame control byte
	//FIXME: see if needs to be non-zero
	write(HEX_CRC_W, 0);

	write(HEX_CRC_W, transaction_seq++);
	if (transaction_seq == 0xff) {
		transaction_seq = 0;
	}
	write(HEX_CRC_W, cmd);
}

static void write_effect_names(void) {
	write(HEX_CRC_W, '[');
	for (uint8_t i = 0; i < effects_len; i++) {
		write(HEX_CRC_W, '"');
		write_pgm_string(HEX_CRC_W, &effects[i].name);
		write(HEX_CRC_W, '"');
		if (i < effects_len - 1) {
			write(HEX_CRC_W, ',');
		};
	}
	write(HEX_CRC_W, ']');
}

static enum zcl_status process_write_cmd(void) {
	bool success = true;
	write_zcl_header(CMDID_WRITE_RESPONSE);

	while(msg_available()) {
		uint16_t attr = read_16(MSG_R);

		if (packet->cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				if (accept(MSG_R, TYPE_BOOLEAN)) {
					uint8_t state = read(MSG_R);
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
				if (accept(MSG_R, TYPE_IEEE_ADDRESS)) {
					mac = read_64(MSG_R);
				}
				break;
			default:
				write_unsupported_attribute(attr);
				success = false;
				break;
			}
		} else if (packet->cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_OPERATING_MODE:
				if (accept(MSG_R, TYPE_ENUM)) {
					uint8_t mode = read(MSG_R);
					set_mode(mode);
				}
				break;
			case ATTR_EFFECT_TEXT:
				if (accept(MSG_R, TYPE_OCTET_STRING)) {
					/*uint8_t slen = read_hex_crc();
					for (uint8_t i = 0; i < slen; i++) {
						effect_text[i] = read_hex_crc(); // TODO
					}*/
				}
				break;
			case ATTR_PLAYLIST:
				if (accept(MSG_R, TYPE_UINT8)) {
					//current_playlist(read_hex_crc());
				}
				break;
			case ATTR_TIMEZONE:
				if (accept(MSG_R, TYPE_INT32)) {
					//TODO			}
				}
				break;
			case ATTR_TIME:
				if (accept(MSG_R, TYPE_UTC_TIME)) {
					//TODO
				}
				break;
			case ATTR_EFFECT:
				if (accept(MSG_R, TYPE_UINT8)) {
					//uint8_t current_effect = read_hex_crc(); //TODO
				}
				break;
			default:
				write_unsupported_attribute(attr);
				success = false;
				break;
			}
		}
	}

	if (success) {
		write(HEX_CRC_W, STATUS_SUCCESS);
	}
	return ZCL_SUCCESS;
}

static void write_default_response(uint8_t cmd, uint8_t status) {
	//write_packet_header();
	write_zcl_header(2); //FIXME: wtf?
	write(HEX_CRC_W, cmd);
	write(HEX_CRC_W, status);
}

static void write_attr_resp_fail(void) {
}

//------ Serial port functions ---------

/**
 * Returns the next packet message byte.
 */
static uint8_t msg_next(void) {
	return packet->msg[msg_i++];
}

/**
 * Is there more of packet message to read.
 * Resets msg_next():s read counter when reaches the end of message.
 */
static bool msg_available(void) {
	if ((packet->length - PACKET_HEADER_LEN) >= msg_i) {
		msg_i = 0;
		return false;
	}
	return true;
}

// Writes

static inline void write(writer_t w, uint8_t data) {
	w(data);
}

static void write_16(writer_t w, uint16_t data) {
	w(data >> 8);
	w(data & 0x00ff);
}

static void write_64(writer_t w, uint64_t data) {
	for (uint8_t i = 0; i < sizeof(data); i++) {
		w(data >> (8 * i));
	}
}

static void write_pgm_string(writer_t w, const char * const* pgm_p) {
	char *p = (char*)pgm_read_word_near(pgm_p);
	char c;

	// If is NULL, print is as zero-length string
	if ( p == NULL) {
		w(' ');
		return;
	}
	
	// Read byte-by-byte and write, not including NUL byte
	c = pgm_read_byte_near(p++);
	while (c != '\0') {
		w(c);
		c = pgm_read_byte_near(p++);
	}
}


/**
 * Sends data to the serial port and updates the write CRC value
 */
static void serial_send_hex_crc(uint8_t data) {
	write_crc = _crc_ccitt_update(write_crc, data);
	serial_send_hex(data);
}

/**
 * Resets the internally used write CRC value
 */
static void reset_write_crc(void) {
	write_crc = 0xffff;
}

/**
 * Hex encodes data and sends it to the serial port
 */
static inline void serial_send_hex(uint8_t data) {
	hex_value_t val;
	val = itohval(data);

	serial_send(val.high);
	serial_send(val.low);
}

// Reads

/**
 * Reads a single byte, compares it to the given parameter
 * and returns if it matches.
 */
static bool accept(reader_t r, uint8_t val) {
	return r() == val;
}

/**
 * Reads and returns single byte from reader_t
 */
static uint8_t read(reader_t r) {
	return r();
}

/**
 * Reads two bytes from reader_t and returns them as a single value
 */
static uint16_t read_16(reader_t r) {
	uint16_t val;
	val = (r() << 8);
	val |= r();
	return val;
}

static uint64_t read_64(reader_t r) {
	uint64_t val = 0x0000000000000000;
	for (uint8_t i = 0; i < 8; i++) {
		val |= (r() << (i * 8));
	}
	return val;
}

/**
 * Convert an integer to hex characters.
 */
static hex_value_t itohval(uint8_t i) {
	hex_value_t val;
	val.high = itoh(i >> 4);
	val.low = itoh(i & 0x0f);
	return val;
}

/**
 * Convert an integer to a single hex character
 */
static uint8_t itoh(uint8_t i) {
	if (i >= 0 && i <= 9) {
		return i;
	}
	if (i >= 10 && i <= 16) {
		return 'A' + (i - 10);
	}

	return NOT_HEX;
}

#endif //AVR_ZCL
