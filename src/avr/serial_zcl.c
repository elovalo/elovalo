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

/* Functions for USART access via packet-based interface. This is used
 * only in ZCL build. */

#ifdef AVR_ZCL

#include <stdint.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <util/atomic.h>
#include "serial_zcl.h"

// RX packet buffer
union zcl_u zcl;

// Internal state
volatile static struct {
	bool ati:1;               // If ATI is received
	bool high_nibble:1;       // If last nibble was "high" nibble
	bool packet_ready:1;      // Packet is received (CRC not checked)
	bool wait_zero:1;         // Expect the next byte to be zero
	bool hex_decoding:1;      // Hex decoder enabled
	bool receipt:1;           // Has received a receipt
	bool ack:1;               // ACK if true, else NAK
	uint8_t i;                // Byte position in receive buffer
} state = {false,false,false,false,false,false,false,0};

/**
 * Called when a byte is received from USART.
 */
ISR(USART_RX_vect)
{
	char x = UDR0; // Read byte

	if (state.wait_zero) {
		/* Does not check if we really received zero. TODO if
		 * it's needed. */
		state.wait_zero = false;
		return;
	}

	switch (x) {
	case 'i':
	case 'I':
		// A hack: Just listening 'I' letter of ATI to keep it simple
		state.ati = true;
		return;
	case 'N':
		state.receipt = true;
		state.ack = false;
		return;
	case 'K':
		state.receipt = true;
		state.ack = true;
		return;
	case 'S':
		if (!state.packet_ready) {
			// If 'S' comes inside message, reset i to beginning
			state.i = 0; 
			state.wait_zero = true;
			state.hex_decoding = true;
		}
	}

	// Do not mess the receive buffer if receiver is off or no 'S' received
	if (!state.hex_decoding) return;

	uint8_t hex;
	if (x >= '0' && x <= '9') {
		hex = x-'0';
	} else if (x >= 'A' && x <= 'F') {
		hex = x-'A'+10;
	} else if (x >= 'a' && x <= 'f') {
		hex = x-'a'+10;
	} else {
		// Invalid character, ignoring
		return;
	}

	// Put the nibble to array
	state.high_nibble = !state.high_nibble;
	if (state.high_nibble) {
		zcl.raw[state.i] |= hex;
		state.i++; // Whole byte ready
	} else {
		zcl.raw[state.i] = hex << 4;
		return;
	}

	/* A whole byte is received and everything done. Now checking
	 * if we are running out of buffer or receiving has
	 * completed. NB! Length and CRC consume equal amount of
	 * bytes, so reading is stopped after packet length matches
	 * index to read CRC as well. */
	if (state.i == ZCL_RX_BUF_SIZE ||
	    (state.i >= 2 && zcl.packet.length == state.i))
	{
		// Stop receiver
		state.packet_ready = true;
		state.hex_decoding = false;
	}
}

bool zcl_packet_available(void)
{
	return state.packet_ready;
}

void zcl_receiver_reset(void)
{
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		// Receipt and ATI are not related to packet receiver
		state.high_nibble = false;
		state.packet_ready = false;
		state.wait_zero = false;
		state.hex_decoding = false;
		// state.i is reset by 'S' case
	}
}

bool zcl_ati(void)
{
	if (!state.ati) return false;
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		state.ati = false;
	}
	return true;
}

bool wait_receipt(void)
{
	// FIXME implement timeout
	while (!state.receipt) {
		sleep_mode();
	}
	reset_receipt();
	return state.ack;
}

void reset_receipt(void) {
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		state.receipt = false;
	}
}

bool zcl_receiver_has_data(void)
{
	return state.ati || state.packet_ready || state.receipt;
}

#endif // AVR_ZCL
