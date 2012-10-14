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

#include "serial.h"
#include "serial_hex.h"
#include "serial_zcl.h"
#include "zcl_commands.h"

// Frame types
#define ZCL_ACK 'K' // Successfully received last packet
#define ZCL_NAK 'N' // Error, please resend last packet
#define ZCL_STX 'S' // Sending a new packet

// Message defines
#define ZCL_CHANNEL 1 // ZCL message channel

#define ZCL_MESSAGE_HEADER_SIZE 13

#define ELOVALO_ENDPOINT 70

//TODO: replace with eeprom read
#define ATI_LEN 35
uint8_t ati_resp[] = "C2IS,elovalo,v1.5,01:23:45:67:89:AB\n";
uint8_t mac[] = "\x01\x23\x45\x67\x89\xAB";

// Private
static void process_packet(void);
static void process_payload(uint16_t length);
static void process_zcl_message(uint16_t length);
static void send_error(void);
static void send_ok(void);
static void check_ATI(void);

void serial_zcl_process(uint8_t cmd) {
	serial_ungetc(cmd); //Don't need it right now
	check_ATI();

	uint8_t frame = serial_read();
	switch (frame) {
		case ZCL_ACK:
			break;
		case ZCL_NAK:
			break;
		case ZCL_STX:
			process_packet();
			break;
	}
}

/**
 * Checks if an ATI response is being sent,
 * and responds with identity information if one is found
 */
void check_ATI(void) {
	uint8_t b = serial_read_blocking();
	uint8_t err = 0;

	if (b != 'A') {
		serial_ungetc(b);
		err = 1;
	}

	b = serial_read_blocking();
	if (b != 'T') {
		serial_ungetc(b);
		err = 1;
	}

	b = serial_read_blocking();
	if (b != 'I') {
		serial_ungetc(b);
		err = 1;
	}
	
	if (err) { return; }

	for (uint8_t i = 0; i < ATI_LEN; i++) {
		serial_send(ati_resp[i]);
	}
}

/**
 * Processes a ZCL packet, sends NAK if CRC check fails
 */
static void process_packet(void) {
	uint16_t length;
	uint16_t message_crc;
	read16_t read16;

	if (serial_read_blocking() != '0') {
		send_error();
		return;
	}

	read16 = serial_read_hex_uint16();
	length = read16.val;

	reset_crc();
	process_payload(length);

	read16 = serial_read_hex_uint16();
	message_crc = read16.val;

	if (message_crc != get_crc()) {
		send_error();
	} else {
		send_ok();
	}
}

/**
 * Processes a single ZCL payload
 */
static void process_payload(uint16_t length) {
	read_t read = serial_read_hex_encoded_crc();
	length--;

	switch (read.byte) {
		case ZCL_CHANNEL:
			process_zcl_message(length);
			break;
		default:
			// TODO: check correct way to handle
			break;
	}
}

static void process_zcl_message(uint16_t length) {
	read_t   read;
	read16_t read16;
	uint16_t profile;
	uint16_t cluster;

	// Checks if MAC address matches
	for (uint8_t i = 0; i < 8; i++) {
		read = serial_read_hex_encoded_crc();
		if (read.byte != mac[i]) {
			// TODO: Check correct way to handle
			return;
		}
	}

	// Checking endpoint ID
	read = serial_read_hex_encoded_crc();
	if (read.byte != ELOVALO_ENDPOINT) {
		return;
	}

	read16 = serial_read_hex_uint16_crc();
	profile = read16.val;

	read16 = serial_read_hex_uint16_crc();
	cluster = read16.val;

	length -= ZCL_MESSAGE_HEADER_SIZE;
	process_command_frame(length);
}

/**
 * Send a NAK error frame to serial port
 */
static void send_error(void) {
	serial_send(ZCL_NAK);
}

/**
 * Send ACK frame to serial port, signaling that the message was received successfully
 */
static void send_ok(void) {
	serial_send(ZCL_ACK);
}
