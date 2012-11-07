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
#include "serial.h"
#include "sleep.h"

// RX packet buffer
union zcl_u zcl;

enum receipt {
	NOTHING,
	ACK,
	NAK
};

// Internal state
volatile static struct {
	bool ati:1;              // If ATI is received
	bool high_nibble:1;      // If last nibble was "high" nibble
	bool packet_ready:1      // Packet is received (CRC not checked)
	bool wait_zero:1;        // Expect the next byte to be zero
	enum receipt_e receipt:2 // If ACK or NAK is received
	uint8_t i;        // Byte position in receive buffer
} state = {false,false,false,false,NOTHING,0};

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
	case '\r':
	case '\n':
		// A hack: Just listening to carriage return after ATI
		state.ati = true;
		return;
	case 'N':
		state.receipt = NAK;
		return;
	case 'K':
		state.receipt = ACK;
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
	} elseif (x >= 'A' && x <= 'F') {
		hex = x-'A'+10;
	} elseif (x >= 'a' && x <= 'f') {
		hex = x-'a'+10;
	} else {
		// Invalid character, ignoring
		return;
	}

	// Put the nibble to array
	state.high_nibble = !state.high_nibble;
	if (state.high_nibble) {
		buf.raw[rx.i] |= hex;
		state.i++; // Whole byte ready
	} else {
		buf.raw[rx.i] = hex << 4;
		return;
	}

	/* A whole byte is received and everything done. Now checking
	 * if we are running out of buffer or receiving has
	 * completed. NB! Length and CRC consume equal amount of
	 * bytes, so reading is stopped after packet length matches
	 * index to read CRC as well. */
	if (state.i == ZCL_RX_BUF_SIZE ||
	    (state.i >= 2 && buf.packet.length == state.i))
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
	// FIXME ATOMIC_FORCEON
	state = {false,false,false,false,NOTHING,0};
}

enum receipt wait_receipt(void)
{
	return NOTHING; // TODO
}

#endif // AVR_ZCL
