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
#include "main.h"

// Lengths
#define ZCL_MESSAGE_HEADER_LEN 11
#define READ_RESP_HEADER_LEN 4
#define MAC_LEN 8
#define READ_BUF_LEN 32

// Frame types
#define ACK 'K' // Successfully received last packet
#define NAK 'N' // Error, please resend last packet
#define STX 'S' // Sending a new packet

// Channels
#define ZCL_CHANNEL '0' // ZCL message channel

// Elovalo end point id
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

uint8_t error_read = 0;

uint8_t read_buffer[READ_BUF_LEN];

uint16_t write_crc = 0xffff;
uint16_t read_crc = 0xffff;

// ZCL Header variables
uint8_t transaction_seq = 0;
uint8_t mac[] = {0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef};

typedef struct {
    unsigned type: 2;
    unsigned manu_specific: 1;
    unsigned direction: 1;
    unsigned disable_def_resp: 1;
    unsigned reserved: 3;
} frame_control_t;

static void send_error(void);
static void send_ok(void);
static uint8_t read_packet(void);
static uint8_t process_payload(uint16_t length);
static uint8_t process_command_frame(uint16_t cluster, uint16_t length);
static void process_read_cmd(uint16_t cluster, uint16_t length);
static uint16_t read_cmd_length(void);
static void write_packet_header(uint16_t length);
static void write_payload_header(void);
static void write_zcl_header(uint8_t cmd);
static void write_effect_names(void);
static void process_write_cmd(uint8_t cluster, uint16_t length);
static void write_default_response(uint8_t cmd, uint8_t status);
static uint8_t read_hex(void);
static uint16_t read_hex_16(void);
static uint8_t read_hex_crc(void);
static uint16_t read_hex_crc_16(void);
static void reset_read_crc(void);
static void reset_write_crc(void);

void process_zcl_frame(void) {
	uint8_t frametype = serial_read_blocking();

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
	//TODO: did packets need CRC checking?
	error_read = 0;
	reset_read_crc();

	uint16_t length = read_hex_16();

	uint8_t channel = read_hex_crc_byte();
	if (channel != ZCL_CHANNEL) {
		// Do nothing or send error?
		return 1;
	}
	uint8_t err = process_payload(length);
	if (err) { return 1; }

	uint16_t msg_crc = read_hex_crc_16();
	if (msg_crc != read_crc || error_read) {
		return 1;
	}

	return 0;
}

static uint8_t process_payload(uint16_t length) {
	// Confirming MAC address
	for (uint8_t i = 0; i < MAC_LEN; i++) {
		if (mac[1] != read_hex_crc()) {
			return 1;
		}
	}

	uint8_t endpoint = read_hex_crc();
	if (endpoint != EP_ID) {
		return 1;
	}

	uint16_t profile = read_hex_crc_16();
	uint16_t cluster = read_hex_crc_16();

	length -= ZCL_MESSAGE_HEADER_LEN;
	return process_zcl_command_frame(cluster, length);
}

static uint8_t process_command_frame(uint16_t cluster, uint16_t length) {
	frame_control_t frame_control;
	uint8_t fc_byte = read_hex_crc();

	frame_control.type = (fc_byte >> 6);
	frame_control.manu_specific = (fc_byte & 0x00100000) >> 5;
	frame_control.direction = (fc_byte & 0x00010000) >> 4;
	frame_control.disable_def_resp = (fc_byte & 0x00001000) >> 3;
	frame_control.reserved = fc_byte;

	if (frame_control.manu_specific) {
		uint16_t manu_spec = read_hex_crc_16();
		length -= 2;
	}
	uint8_t trans_seq = read_hex_crc();
	uint8_t cmd = read_hex_crc();
	length -= 3;

	process_command(cluster, cmd, length);
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
	uint16_t resp_len = read_cmd_length();
	uint16_t attr;

	write_packet_header(resp_len);
	write_payload_header();
	write_zcl_header(CMDID_READ_RESPONSE);
	
	for (uint8_t i = 0; i < len; i++) {
		attr = read_hex_crc_16();
		if (cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				write_attr_resp_header(ATTR_DEVICE_ENABLED, TYPE_BOOLEAN); //TODO
				//send_hex_encoded(IS_ON); //TODO
				break;
			case ATTR_ALARM_MASK:
				write_attr_resp_header(ATTR_ALARM_MASK, TYPE_BOOLEAN);
				//send_hex_encoded(ALARM_MASK); //TODO
				break;
			case ATTR_IEEE_ADDRESS:
				write_attr_resp_header(ATTR_IEEE_ADDRESS, TYPE_IEEE_ADDRESS);
				for (uint8_t i = 0; i < MAC_LEN; i++) {
					send_hex_encoded(mac[i]);
				}
				break;
			default:
				write_attr_resp_fail(); // TODO
				break;
			}
		} else if (cluster == CLUSTERID_ELOVALO) {
			switch(attr) {
			case ATTR_OPERATING_MODE:
			{
				write_attr_resp_header(ATTR_OPERATING_MODE, TYPE_ENUM);
				uint8_t mode = main_current_mode();
				if (mode == MODE_OFF || mode == MODE_IDLE) {
					send_hex_encoded(0);
				} else if (mode == MODE_EFFECT) {
					send_hex_encoded(1);
				} else if (mode == MODE_PLAYLIST) {
					send_hex_encoded(2);
				}
				break;
			}
			/*case ATTR_EFFECT_TEXT:
				write_attr_resp_header(ATTR_EFFECT_TEXT, TYPE_OCTET_STRING);
				write_effect_text(); //TODO
				break;
			case ATTR_PLAYLIST:
				write_attr_resp_header(ATTR_PLAYLIST, TYPE_UINT8);
				send_hex_encoded(current_playlist);
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
				send_hex_encoded(current_effect);
				break;
			case ATTR_HW_VERSION:
				write_attr_resp_header(ATTR_HW_VERSION, TYPE_OCTET_STRING);
				write_hw_version(); //TODO
				break;
			case ATTR_SW_VERSION:
				write_attr_resp_header(ATTR_SW_VERSION, TYPE_OCTET_STRING);
				write_sw_version(); //TODO
				break;
			default:
				write_attr_resp_fail(); // TODO
				break;*/
			}
		}
	}
	
	write_crc_out();
}

static uint16_t read_cmd_length(uint16_t cluster, uint16_t msg_len) {
	uint16_t length = 1; // Because payload contains the channel byte

	for (uint16_t i = 0; i < msg_len; i += 2) {
		attr = read_hex_crc_16();
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
			switch(cmd) {
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
	//TODO check if crc needed, and the correct order for these
	reset_write_crc();
	send_hex_encoded(STX);
	write_hex_16(length);
	write_hex_crc(ZCL_CHANNEL);
}

static void write_payload_header(void) {
	// write MAC
	for (uint8_t i = 0; i < MAC_LEN; i++) {
		write_hex_crc(mac[i]);
	}

	write_hex_crc(EP_ID);
	write_hex_crc_16(profile); //TODO: profile?
	write_hex_crc_16(CLUSTERID_ELOVALO);
}

static void write_zcl_header(uint8_t cmd){
	write_hex_crc(frame_control);
	write_hex_crc(transaction_seq++);
	if (transaction_seq == 0xff) {
		transaction_seq = 0;
	}
	write_hex_crc(cmd);
}

static void write_effect_names(void) {
	send_hex_encoded('[');
	for (uint8_t i = 0; i < effects_len; i++ ) {
		send_hex_encoded('"');
		send_string_from_pgm(&effects[i].name)
		send_hex_encoded('"');
		if (i < effects_len - 1) {
			send_hex_encoded(',');
		};
	}
	send_hex_encoded(']');
}

static void process_write_cmd(uint8_t cluster, uint16_t length) {
	for (uint8_t i = 0; i < len; i++) {
		attr = read_hex_crc_16();
		if (cluster == CLUSTERID_BASIC) {
			switch(attr) {
			case ATTR_DEVICE_ENABLED:
				if (accept(TYPE_BOOLEAN)) {
					uint8_t state = read_hex_crc();
					if (state == VAL_TRUE) {
						set_mode(MODE_PLAYLIST);
					} else if (state == VAL_FALSE) {
						set_mode(MODE_OFF);
					}
				}
				break;
			case ATTR_ALARM_MASK:
				//TODO
				break;
			case ATTR_IEEE_ADDRESS:
				if (accept(TYPE_IEEE_ADDRESS)) {
					for (uint8_t i = 0; i < MAC_LEN; i++) {
						mac[i] = read_hex_crc();
					}
				}
				break;
			}
		} else if (cluster == CLUSTERID_ELOVALO) {
			switch(cmd) {
			case ATTR_OPERATING_MODE:
				if (accept(TYPE_ENUM)) {
					uint8_t mode = read_hex_crc();
					set_mode(mode);
				}
				break;
			case ATTR_EFFECT_TEXT:
				if (accept(TYPE_OCTET_STRING)) {
					uint8_t slen = read_hex_crc();
					for (uint8_t i = 0; i < slen; i++) {
						effect_text[i] = read_hex_crc(); // TODO
					}
				}
				break;
			case ATTR_PLAYLIST:
				if (accept(TYPE_UINT8)) {
					current_playlist(read_hex_crc());
				}
				break;
			case ATTR_TIMEZONE:
				if (accept(TYPE_INT32)) {
					//TODO			}
				break;
			case ATTR_TIME:
				if (accept(TYPE_UTC_TIME)) {
					//TODO
				}
				break;
			case ATTR_EFFECT:
				if (accept(TYPE_UINT8)) {
					current_effect = read_hex_crc(); //TODO
				}
				break;
			}
		}
	}
	
	write_success_write_resp();
}

static void write_default_response(uint8_t cmd, uint8_t status) {
	write_zcl_header(2);
	write_hex_crc(cmd);
	write_hex_crc(status);
}

static uint8_t read_hex(void) {
	read_t read = serial_hex_read();
	if (!read.good) {
		error_reading |= 1;
	}
	return read.byte
}

static uint16_t read_hex_16(void) {
	read16_t read = serial_hex_read_uint16();
	if (!read.good) {
		error_reading |= 1;
	}
	return read.val;
}

static uint8_t read_hex_crc(void) {
	uint8_t byte = read_hex();
	read_crc = _crc_ccitt_update(read_crc, byte);
	if (!read.good) {
		error_reading |= 1;
	}
	return read.byte
}

static uint16_t read_hex_crc_16(void) {
	read16_t read = serial_hex_read_uint16();
	if (!read.good) {
		error_reading |= 1;
	}
	return read.val;
}

static void reset_read_crc(void) {
	read_crc = 0xffff;
}

static void reset_write_crc(void) {
	write_crc = 0xffff;
}
