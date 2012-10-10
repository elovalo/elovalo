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
#include <util/crc16.h>

#include "serial.h"
#include "serial_hex.h"
#include "serial_zcl.h"

//TODO: replace with eeprom read
#define ATI_LEN 35
uint8_t ep_id = 70;
uint8_t ati_resp[] = "C2IS,elovalo,v1.0,01:23:45:67:89:AB\n";
uint8_t mac[] = "\x01\x23\x45\x67\x89\xAB";

void receive(void) {
	if(serial_available()) {
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
}

void check_ATI(void) {
    uint8_t b = serial_read_blocking();
    if (b != 'A') { serial_ungetc(b); return; }

    b = serial_read_blocking();
    if (b != 'T') { serial_ungetc(b); return; }

    b = serial_read_blocking();
    if (b != 'I') { serial_ungetc(b); return; }

    for (uint8_t i = 0; i < ATI_LEN; i++) {
        serial_send(ati_resp[i]);
    }
}

void process_packet(void) {
	uint16_t len;
	uint16_t crc;
    uint16_t msg_crc;
	read_t read;

    read.byte = serial_read_blocking();
	if (read.byte != '0') { send_error(); } // Not a ZCL compliant packet

    len = read_packet_length();
    crc = process_message(len);

    read = serial_read_hex_encoded();
    msg_crc = (read.byte << 8);
    read = serial_read_hex_encoded();
    msg_crc |= read.byte;

    if (crc != msg_crc) {
        send_error();
    } else {
        send_ok();
    }
}

uint16_t process_message(uint16_t len) {
    read_t read;
    uint16_t crc;
    crc = 0xffff;

    for (uint16_t i = 0; i < len; i++) {
        read = serial_read_hex_encoded();
        if (!read.good) { send_error(); return 0; }

        crc = _crc_ccitt_update(crc, read.byte);

        if (i == 0) {
            // Only ZCL messages supported, no FT
            if (read.byte != ZCL_CHAN) { return 0; }
        } else if (i >= 1 && i <= 9) {
            // MAC
            if (read.byte != mac[i]) { send_error(); return 0; }
        } else if (i == 9) {
            // End point ID
            if (read.byte != ep_id) { send_error(); return 0; }
        } else if (i >= 10 && i <= 11) {
            // TODO: Profile
        } else if (i >= 12 && i <= 13) {
            // TODO: Cluster
        } else {
            // TODO: Payload
        }
    }

    return crc;
}


uint16_t read_packet_length(void) {
	uint16_t len;
	read_t read;

    read = serial_read_hex_encoded();
    len = (read.byte << 8);

    read = serial_read_hex_encoded();
    len |= read.byte;

    return len;
}

void send_error(void) {
    serial_send(ZCL_NAK);
}

void send_ok(void) {
    serial_send(ZCL_ACK);
}
