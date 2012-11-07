/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
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
	unsigned type: 2;
	unsigned mfr_specific: 1;
	unsigned direction: 1;
	unsigned disable_def_resp: 1;
	unsigned reserved: 3;
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

#endif /* SERIAL_ZCL_H_ */
