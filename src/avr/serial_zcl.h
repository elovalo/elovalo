/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Functions for USART access using ZCL packet buffer */

#ifndef SERIAL_ZCL_H_
#define SERIAL_ZCL_H_

#include <stdint.h>
#include <stdbool.h>

#define ZCL_RX_BUF_SIZE 128
#define TXRX_OK 0
#define TXRX_OVERFLOW 1

struct packet_s {
	uint16_t length;
	uint8_t channel;
	uint64_t mac;
	uint8_t endpoint;
	uint16_t profile;
	uint16_t cluster;
	uint8_t type: 2;
	bool mfr_specific: 1;
	bool direction: 1;
	bool disable_def_resp: 1;
	uint8_t reserved: 3;
	// Manufacturer code is never used, skipping
	uint8_t transaction_id;
	uint8_t cmd_type;
	uint8_t msg[];
};

union zcl_u {
	uint8_t raw[ZCL_RX_BUF_SIZE];
	struct packet_s packet;
};

extern union zcl_u zcl;

/**
 * Returns true if ZCL packet is received and zcl global variable is
 * considered stable to use. This function does not validate CRC, you
 * are responsible to do that.
 */
bool zcl_packet_available(void);

/**
 * Reset receiver state. After reset it starts receiving packet again.
 */
void zcl_receiver_reset(void);

/**
 * Waits ACK or NAK from serial line and returns true if ACK is
 * received. Timeout is not implemented but when it shall be, false is
 * returned if timeout occurs. This doesn't wait for receipt if one is
 * already received, so it's safe to do anything between sending the
 * message and calling this function. This function automatically
 * resets its internal receipt status flag.
 */
bool wait_receipt(void);

/**
 * Returns true if ATI has been received. This command resets ATI
 * atomically back to false.
 */
bool zcl_ati(void);

#endif /* SERIAL_ZCL_H_ */
