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
#include "clock.h"
#include "../common/pgmspace.h"
#include "main.h"
#include "serial_zcl.h"
#include "../generated/effect_constants.h"
#include "../common/playlists.h"
#include "../common/time.h"

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
#define CMDID_DEFAULT_RESPONSE 0x0b

// Attributes
// Basic cluster
#define ATTR_DEVICE_ENABLED 0x0012
#define ATTR_ALARM_MASK 0x0013

// Elovalo cluster
#define ATTR_IEEE_ADDRESS 0x12
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
#define STATUS_UNSUP_CLUSTER_COMMAND 0x81
#define STATUS_UNSUP_GENERAL_COMMAND 0x82
#define STATUS_UNSUPPORTED_ATTRIBUTE 0x86
#define STATUS_INVALID_VALUE 0x87
#define STATUS_INVALID_DATA_TYPE 0x8d
#define STATUS_READ_ONLY 0x88

// Values
#define BOOL_TRUE 0x01
#define BOOL_FALSE 0x00

// Serial port
#define NOT_HEX 'G'

// Read buffer defines
#define READ_BUF_CAPACITY 32
#define READ_BUF_OK 0x00
#define READ_BUF_OVERFLOW 0x01

// ZigBee time starts at Sat Jan 01 00:00:00 UTC 2000
#define ZIGBEE_TIME_OFFSET 946684800

enum zcl_status {
	ZCL_SUCCESS,
	ZCL_BAD_PROFILE,
	ZCL_BAD_ENDPOINT,
	ZCL_BAD_COMMAND,
	ZCL_IMPOSSIBLE
};

static void process_payload();
static bool process_cmd_frame();
static bool process_read_cmd();
static void send_attr_resp_header(uint16_t attr, uint8_t type);

static void send_packet_header(uint16_t length);
static void send_zcl_header(uint8_t cmd);
static void send_effect_names(void);
static bool process_write_cmd(void);
static void send_default_response(uint8_t cmd, uint8_t status);
static void send_cmd_status(uint16_t attr, uint8_t status);

static void send_16(uint16_t);
static void send_16_without_crc(uint16_t);
static void send_32(uint32_t);
static void send_i32(int32_t);
static void send_64(uint64_t);
static void send_pgm_string(const char * const* pgm_p);

static void send_payload(uint8_t);
static void reset_send_crc(void);
static inline void serial_send_hex(uint8_t);

static bool msg_available(void);
static uint8_t msg_get(void);
static uint16_t msg_get_16(void);
static uint32_t msg_get_32(void);
static uint32_t msg_get_i32(void);
static uint64_t msg_get_64(void);

static uint8_t itoh(uint8_t i);

static void reset_msg_ptr(void);

static bool dry_run;
static uint16_t response_length;

/* Helper for local PROGMEM string writing. sizeof() includes \0 and
 * ZigBee has no terminator, therefore substracting 1 from length. */
#define send_local_pgm_str(s) send_local_pgm_str_(s,sizeof(s)-1)
static void send_local_pgm_str_(const char *s, uint8_t len);

uint16_t send_crc = 0xffff;
void *msg_i; // Packet message read index

// MAC in original byte order (not reversed like in XML format)
uint64_t mac = 0x0123456789abcdef;

// Some version-specific constants
PROGMEM static const char ati_resp[] = "C2IS,elovalo,v1.5,01:23:45:67:89:AB:CD:EF\n";
PROGMEM static const char sw_resp[] = "0.2012.11.15";
PROGMEM static const char hw_resp[] = "EV-1-C2";

void process_serial(void)
{
	if (zcl_ati()) {
		// ATI command response
		for (uint8_t i = 0; i < sizeof(ati_resp)-1; i++) {
			char c = pgm_get(ati_resp[i],byte);
			serial_send(c);
		}
		// May continue to packet processing
	}

	if (zcl_own_fault()) {
		/* If buffer overflow or other internal error
		 * happened, there is not much to do. TODO Maybe there
		 * should be some internal flag? Or proper ZigBee
		 * error? */
		serial_send(ACK);
		zcl_receiver_reset();
		return;
	}	

	if (!zcl_packet_available()) return;

	// We have a packet. Checking CRC.
	uint16_t *msg_crc = (uint16_t *)(zcl.raw + zcl.packet.length + 2);
	uint16_t crc = 0xffff;
	for (uint16_t i = 0; i < zcl.packet.length; i++) {
		crc = _crc_xmodem_update(crc, zcl.raw[i+2]);
	}

	/* Answering ACK and processing the answer if it was
	 * correct. Otherwise just send NAK and let the sender to
	 * resend it later */
	if (*msg_crc == crc) {
		serial_send(ACK);
		process_payload();
	} else {
		serial_send(NAK);
	}
	
	// Re-enable the receiver
	zcl_receiver_reset();
}

static void process_payload(void) {

	// Only ZCL messages are supported.
	if (zcl.packet.channel != ZCL_CHANNEL) return;

	// Filter out messages that do not belong to me
	if (zcl.packet.mac != mac) {
		return;
	}

	/* If using manufacturer specific extensions, do not
	 * touch. The payload is distorted anyway */
	if (zcl.packet.mfr_specific) return;

	/* Determining message length */
	dry_run = true;
	response_length = 0;
	bool has_response = process_cmd_frame();
	if (!has_response) return; // Just say ACK

	/* Sending actual response */
	dry_run = false;
	send_packet_header(response_length);
	reset_send_crc();
	process_cmd_frame();
	send_16_without_crc(send_crc);
}

/**
 * Processes command frame. If no response should be generated,
 * returns false. This function is ran twice. First for length
 * calculation and the second run runs the "real" action.
 */
static bool process_cmd_frame(void) {
	/* Start message reading from the beginning */
	reset_msg_ptr();

	/* Filter out profiles and endpoints that are not supported on
	 * this device. FIXME: Generate error responses for these. */
	if (zcl.packet.endpoint != ENDPOINT) {
		// TODO generate error msg
		return false;
	}
	if (zcl.packet.profile != PROFILE) {
		// TODO generate error msg
		return false;
	}
	switch (zcl.packet.cmd_type) {
	case CMDID_READ:
		return process_read_cmd();
	case CMDID_WRITE:
		process_write_cmd();
		return true;
	case CMDID_DEFAULT_RESPONSE:
		return false;
	default:
		// FIXME: See if correct way to handle unsupport command type
		if (!zcl.packet.disable_def_resp) {
			send_default_response(zcl.packet.cmd_type,
				STATUS_UNSUP_GENERAL_COMMAND);
		}
		return true;
	}
}

static bool process_read_cmd() {
	send_zcl_header(CMDID_READ_RESPONSE);

	while(msg_available()) {
		uint16_t attr;
		attr = msg_get_16();
		
		if (zcl.packet.cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				send_attr_resp_header(ATTR_DEVICE_ENABLED, TYPE_BOOLEAN);
				send_payload(get_mode());
				break;
			case ATTR_ALARM_MASK:
				send_attr_resp_header(ATTR_ALARM_MASK, TYPE_BOOLEAN);
				send_payload(0); //FIXME: implement
				break;
			default:
				send_cmd_status(attr, STATUS_UNSUPPORTED_ATTRIBUTE);
				break;
			}
		} else if (zcl.packet.cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_IEEE_ADDRESS:
			{
				send_attr_resp_header(ATTR_IEEE_ADDRESS, TYPE_IEEE_ADDRESS);
				send_64(mac);
				break;
			}
			case ATTR_OPERATING_MODE:
			{
				send_attr_resp_header(ATTR_OPERATING_MODE, TYPE_ENUM);
				uint8_t mode = get_mode();
				if (mode == MODE_SLEEP || mode == MODE_IDLE) {
					send_payload(0);
				} else if (mode == MODE_EFFECT) {
					send_payload(1);
				} else if (mode == MODE_PLAYLIST) {
					send_payload(2);
				}
				break;
			}
			/*
			case ATTR_EFFECT_TEXT:
				send_attr_resp_header(ATTR_EFFECT_TEXT, TYPE_OCTET_STRING);
				write_effect_text(); //TODO
				break;
			*/
			case ATTR_PLAYLIST:
				send_attr_resp_header(ATTR_PLAYLIST, TYPE_UINT8);
				send_payload(active_playlist);
				break;
			case ATTR_TIMEZONE:
				send_attr_resp_header(ATTR_TIMEZONE, TYPE_INT32);
				send_i32(get_timezone());
				break;
			case ATTR_TIME:
				send_attr_resp_header(ATTR_TIME, TYPE_UTC_TIME);
				send_32(time(NULL)-ZIGBEE_TIME_OFFSET);
				break;
			case ATTR_EFFECT_NAMES:
				send_attr_resp_header(ATTR_EFFECT_NAMES,
					TYPE_LONG_OCTET_STRING);
				send_effect_names();
				break;
			/*
			case ATTR_PLAYLIST_NAMES:
				send_attr_resp_header(ATTR_PLAYLIST_NAMES, TYPE_LONG_OCTET_STRING);
				write_playlist_names(); //TODO
				break;
			*/
			case ATTR_PLAYLIST_EFFECTS:
			{
				send_attr_resp_header(ATTR_PLAYLIST_EFFECTS, TYPE_OCTET_STRING);
				 // current playlist index
				uint8_t pl_begin = pgm_get(playlists[active_playlist], byte);
				// End index to playlist, not included to playlist
				uint8_t pl_end;

				if (active_playlist == playlists_len - 1) {
					pl_end = master_playlist_len;
				} else {
					pl_end = pgm_get(playlists[active_playlist + 1], byte);
				}
				//Send string length
				send_payload(pl_end - pl_begin);
				for (uint8_t i = pl_begin; i < pl_end; i++) {
					send_payload(pgm_get(master_playlist[i].id, byte));
					//send_payload(i);
				}

				break;
			}
			case ATTR_EFFECT:
				send_attr_resp_header(ATTR_EFFECT, TYPE_UINT8);
				send_payload(active_effect);
				break;
			case ATTR_HW_VERSION:
				send_attr_resp_header(ATTR_HW_VERSION, TYPE_OCTET_STRING);
				send_local_pgm_str(hw_resp);
				break;
			case ATTR_SW_VERSION:
				send_attr_resp_header(ATTR_SW_VERSION, TYPE_OCTET_STRING);
				send_local_pgm_str(sw_resp);
				break;
			default:
				send_cmd_status(attr, STATUS_UNSUPPORTED_ATTRIBUTE);
				break;
			}
		} else {
			// FIXME: See if correct way to handle incorrect cluster
			if (!zcl.packet.disable_def_resp) {
				send_default_response(CMDID_READ,
					STATUS_UNSUP_CLUSTER_COMMAND);
			}
			return true;
		}
	}
	return true;
}

static void send_attr_resp_header(uint16_t attr, uint8_t type) {
	send_16(attr);
	send_payload(STATUS_SUCCESS);
	send_payload(type);
}

static void send_cmd_status(uint16_t attr, uint8_t status) {
	send_16(attr);
	send_payload(status);
}

static void send_packet_header(uint16_t length) {
	serial_send(STX);
	serial_send(PACKET_BEGIN);
	send_16_without_crc(length);
}

static void send_zcl_header(uint8_t cmd) {
	send_payload(ZCL_CHANNEL);
	send_64(mac);

	send_payload(zcl.packet.endpoint);
	send_16(zcl.packet.profile);
	send_16(zcl.packet.cluster);

	// Send out the frame control byte
	//FIXME: see if needs to be non-zero
	send_payload(0);

	send_payload(zcl.packet.transaction_id);
	send_payload(cmd);
}

static void send_effect_names(void) {
	send_16(EFFECT_JSON_LEN);
	send_payload('[');
	for (uint8_t i = 0; i < effects_len; i++) {
		send_payload('"');
		send_pgm_string(&effects[i].name);
		send_payload('"');
		if (i < effects_len - 1) {
			send_payload(',');
		};
	}
	send_payload(']');
}

static bool process_write_cmd(void) {
	bool success = true;
	send_zcl_header(CMDID_WRITE_RESPONSE);

	while(msg_available()) {
		uint16_t attr = msg_get_16();

		if (zcl.packet.cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				if (msg_get() == TYPE_BOOLEAN) {
					uint8_t state = msg_get();
					if (state == BOOL_TRUE) {
						set_mode(MODE_PLAYLIST);
					} else if (state == BOOL_FALSE) {
						set_mode(MODE_IDLE);
					}
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			case ATTR_ALARM_MASK:
				//TODO
				break;
			default:
				send_cmd_status(attr, STATUS_UNSUPPORTED_ATTRIBUTE);
				success = false;
				break;
			}
		} else if (zcl.packet.cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_IEEE_ADDRESS:
				if (msg_get() == TYPE_IEEE_ADDRESS) {
					mac = msg_get_64();
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			case ATTR_OPERATING_MODE:
				if (msg_get() == TYPE_ENUM) {
					uint8_t mode = msg_get();
					set_mode(mode);
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			case ATTR_EFFECT_TEXT:
				if (msg_get() == TYPE_OCTET_STRING) {
					/*uint8_t slen = read_hex_crc();
					for (uint8_t i = 0; i < slen; i++) {
						effect_text[i] = read_hex_crc(); // TODO
					}*/
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			case ATTR_PLAYLIST:
				if (msg_get() == TYPE_UINT8) {
					change_playlist(msg_get());
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			case ATTR_TIMEZONE:
			{
				if (msg_get() == TYPE_INT32) {
					set_timezone(msg_get_i32());
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			}
			case ATTR_TIME:
				if (msg_get() == TYPE_UTC_TIME) {
					time_t t = msg_get_32()+ZIGBEE_TIME_OFFSET;
					stime(&t);
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			case ATTR_EFFECT:
				if (msg_get() == TYPE_UINT8) {
					change_current_effect(msg_get());
				} else {
					success = false;
					send_cmd_status(attr, STATUS_INVALID_DATA_TYPE);
				}
				break;
			default:
				send_cmd_status(attr, STATUS_UNSUPPORTED_ATTRIBUTE);
				success = false;
				break;
			}
		} else {
			// FIXME: See if correct way to handle incorrect cluster
			if (!zcl.packet.disable_def_resp) {
				send_default_response(CMDID_WRITE,
					STATUS_UNSUP_CLUSTER_COMMAND);
			}
			return true;
		}
	}

	// If no error reports has been written
	if (success) {
		send_payload(STATUS_SUCCESS);
	}
	return true;
}

static void send_default_response(uint8_t cmd, uint8_t status) {
	send_zcl_header(CMDID_DEFAULT_RESPONSE);
	send_payload(cmd);
	send_payload(status);
}

//------ Serial port functions ---------

// Send functions write hex encoded data to the serial port and update
// the internal CRC value
static void send_16(uint16_t data) {
	send_payload(data & 0x00ff);
	send_payload(data >> 8);
}

// Same as send_16 but does not update the CRC
static void send_16_without_crc(uint16_t data) {
	serial_send_hex(data & 0x00ff);
	serial_send_hex(data >> 8);
}

static void send_32(uint32_t data) {
	for (uint8_t i = 0; i < 4; i++) {
		send_payload(data >> (8 * i));
	}
}

static void send_i32(int32_t data) {
	for (uint8_t i = 0; i < sizeof(int32_t); i++) {
		send_payload(data >> (8 * i));
	}
}

static void send_64(uint64_t data) {
	for (uint8_t i = 0; i < 8; i++) {
		send_payload(data >> (8 * i));
	}
}

static void send_pgm_string(const char * const* pgm_p) {
	char *p = (char*)pgm_read_word_near(pgm_p);
	char c;

	// If is NULL, print is as zero-length string
	if ( p == NULL) {
		send_payload(' ');
		return;
	}
	
	// Read byte-by-byte and write, not including NUL byte
	c = pgm_read_byte_near(p++);
	while (c != '\0') {
		send_payload(c);
		c = pgm_read_byte_near(p++);
	}
}


/**
 * Hex encodes data and then sends it to the serial port while updating the
 * send_crc value. When dry_run is true, no data is sent, only added to the
 * response_length counter.
 */
static void send_payload(uint8_t data) {
	if (dry_run) {
		response_length++;
	} else {
		send_crc = _crc_xmodem_update(send_crc, data);
		serial_send_hex(data);
	}
}

/**
 * Resets the internally used send CRC value
 */
static void reset_send_crc(void) {
	send_crc = 0xffff;
}

/**
 * Hex encodes data and sends it to the serial port
 */
static inline void serial_send_hex(uint8_t data) {
	serial_send(itoh(data >> 4));
	serial_send(itoh(data & 0x0f));
}

// Message reading

/**
 * Returns true if there data left in a packet.
 */
static bool msg_available(void)
{
	void *end = zcl.packet.msg + zcl.packet.length - PACKET_HEADER_LEN;
	return msg_i < end;
}

/**
 * Reads and returns single byte from message buffer
 */
static uint8_t msg_get(void) {
	uint8_t p = *(uint8_t *)msg_i;
	msg_i += sizeof(uint8_t);
	return p;
}

static uint16_t msg_get_16(void) {
	uint16_t p = *(uint16_t *)msg_i;
	msg_i += sizeof(uint16_t);
	return p;
}

static uint32_t msg_get_32(void) {
	uint32_t p = *(uint32_t *)msg_i;
	msg_i += sizeof(uint32_t);
	return p;
}

static uint32_t msg_get_i32(void) {
	uint32_t p = *(int32_t *)msg_i;
	msg_i += sizeof(int32_t);
	return p;
}

static uint64_t msg_get_64(void) {
	uint64_t p = *(uint64_t *)msg_i;
	msg_i += sizeof(uint64_t);
	return p;
}

/**
 * Convert an integer to a single hex character
 */
static uint8_t itoh(uint8_t i) {
	if (i >= 0 && i <= 9) {
		return i + '0';
	}
	if (i >= 10 && i <= 16) {
		return 'A' + (i - 10);
	}

	return NOT_HEX;
}

/**
 * Reset packet pointer used in msg_available() loops
 */
static void reset_msg_ptr(void)
{
	msg_i = zcl.packet.msg;
}

static void send_local_pgm_str_(const char *s, uint8_t len)
{
	send_payload(len);
	for (uint8_t i = 0; i < len; i++) {
		char c = pgm_get(s[i],byte);
		send_payload(c);
	}
}

#endif //AVR_ZCL
