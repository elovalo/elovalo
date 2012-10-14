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

#include <avr/wdt.h>
#include "serial_hex.h"

// Command Identifier Field Values
#define CMDID_READ_ATTRS			0x00
#define CMDID_READ_ATTRS_RESP		0x01
#define CMDID_WRITE_ATTRS			0x02
#define CMDID_WRITE_ATTRS_UNDIVIDED	0x03
#define CMDID_WRITE_ATTRS_RESP		0x04
#define CMDID_WRITE_ATTRS_NO_RESP	0x05
#define CMDID_CONF_REP				0x06
#define CMDID_CONF_REP_RESP			0x07
#define CMDID_READ_REP_CONF			0x08
#define CMDID_READ_REP_CONF_RESP	0x09
#define CMDID_REP_ATTRS				0x0a

// Cluster IDs
#define CLUSTERID_BASIC			0x00
#define CLUSTERID_ELOVALO		0x500

// Attribute IDs
// CLUSTERID_BASIC
#define ATTR_DEVICE_ENABLED	0x0012
#define ATTR_ALARM_MASK		0x0013

// CLUSTERID_ELOVALO
#define ATTR_IEEE_ADDR			0x401
#define ATTR_OPERATING_MODE		0x01
#define ATTR_EFFECT_TEXT		0x02
#define ATTR_PLAYLIST			0x03
#define ATTR_TIMEZONE			0x04
#define ATTR_TIME				0x05
#define ATTR_EFFECT_NAMES		0x06
#define ATTR_PLAYLIST_NAMES		0x07
#define ATTR_PLAYLIST_EFFECTS	0x08
#define ATTR_EFFECT				0x09
#define ATTR_HW_VERSION			0x10
#define ATTR_SW_VERSION			0x11

// Command directions
#define DIR_SERVER_TO_CLIENT 1
#define DIR_CLIENT_TO_SERVER 0



// Private
static uint8_t frame_type(uint8_t fc);
static uint8_t is_manuf_specific(uint8_t fc);
static uint8_t frame_direction(uint8_t fc);
static uint8_t is_default_resp_disabled(uint8_t fc);

void process_read_command(uint16_t length);
void process_read_response(uint16_t length);
void process_write_command(uint16_t length);
void process_write_response(uint16_t length);

void process_command_frame(uint16_t length) {
	read_t   read;
	read16_t read16;
	uint8_t  frame_control;
	uint16_t manu_spec;
	uint8_t  transact_seq;
	uint8_t  cmd_id;

	read = serial_read_hex_encoded_crc();
	frame_control = read.byte;
	
	if (is_manuf_specific(frame_control)) {
		read16 = serial_read_hex_uint16_crc();
		manu_spec = read16.val;
		length -= 2;
	}
	read = serial_read_hex_encoded_crc();
	transact_seq = read.byte;

	read = serial_read_hex_encoded_crc();
	cmd_id = read.byte;
	length -= 3;

	switch (cmd_id) {
	case CMDID_READ_ATTRS:
		process_read_command(length);
	case CMDID_READ_ATTRS_RESP:
		process_read_response(length);
	case CMDID_WRITE_ATTRS:
		process_write_command(length);
	case CMDID_WRITE_ATTRS_RESP:
		process_write_response(length);
	}
}

void process_read_command(uint16_t length) {
	//TODO: implement
}

void process_read_response(uint16_t length) {
	//TODO: implement
}

void process_write_command(uint16_t length) {
	//TODO: implement
}

void process_write_response(uint16_t length) {
	//TODO: implement
}
/**
 * Checks the frame control byte for the frame type
 */
uint8_t frame_type(uint8_t fc) {
	return (fc >> 6);
}

/**
 * Checks frame control byte if the command is
 * manufacturer specific or not
 */
uint8_t is_manuf_specific(uint8_t fc) {
	return (fc & (1 << 5)) != 0;
}

/**
 * Checks the frame control byte for the direction of the
 * command.
 */
uint8_t frame_direction(uint8_t fc) {
	if ((fc & (1 << 4)) != 0) {
		return DIR_SERVER_TO_CLIENT;
	} else {
		return DIR_CLIENT_TO_SERVER;
	}
}

/**
 * Checks the frame control byte if default response
 * is accepted.
 */
uint8_t is_default_resp_disabled(uint8_t fc) {
	return (fc & (1 << 3)) != 0;
}
