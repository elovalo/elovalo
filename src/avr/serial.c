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

/* Functions for USART access */

#include "sleep.h"
#include <util/atomic.h>
#include "serial.h"

// TX ring buffer
static uint8_t tx_buf[TX_BUF_SIZE];
static uint8_t tx_in_i = 0;
volatile static uint8_t tx_out_i = 0; // Set by USART_TX_vect

// Transmitter state
volatile static uint8_t tx_state = TXRX_OK;

/**
 * Called when USART has finished transmit.
 */
ISR(USART_TX_vect)
{
	// If no data in buffer, then we just bail out.
	if (tx_in_i == tx_out_i) return;

	// If it overflows, do not fill console with garbage.
	if (tx_state == TXRX_OVERFLOW) return;

	// Send the byte and wrap to start if needed
	UDR0 = tx_buf[tx_out_i++];
	if (tx_out_i == TX_BUF_SIZE) tx_out_i = 0;
}

void serial_TX_empty(void) {
	tx_in_i = tx_out_i;
	tx_state = TXRX_OK;
}

uint8_t serial_send_available(void) {
	uint8_t diff = tx_out_i - tx_in_i - 1;
	return (tx_out_i < tx_in_i + 1) ? diff + TX_BUF_SIZE : diff;
}

void serial_send_nonblocking(uint8_t data)
{
	ATOMIC_BLOCK(ATOMIC_FORCEON) {

		/* If buffer is empty and no byte is in transit, do not queue
		 * at all. */
		if (tx_in_i == tx_out_i && (UCSR0A & (1<<UDRE0))) {
			UDR0 = data;
			return;
		}

		tx_buf[tx_in_i++] = data;

		// Wrap to start
		if (tx_in_i == TX_BUF_SIZE) tx_in_i = 0;

		if (tx_in_i == tx_out_i) {
			// Overflow condition
			tx_state = TXRX_OVERFLOW;
		}
	}
}

void serial_send(uint8_t data) {
	while ((tx_in_i+1 == tx_out_i) ||
		   (tx_in_i == TX_BUF_SIZE-1 && tx_out_i == 0));
	serial_send_nonblocking(data);
}

/* Receiver functions. Conditionally compiled only for elocmd
 * target. ZCL versions are in serial_zcl.c */
#ifdef AVR_ELO

// RX ring buffer
uint8_t rx_buf[RX_BUF_SIZE];
volatile uint8_t rx_in_i = 0; // Set by USART_RX_vect
uint8_t rx_out_i = 0;

// Receiver state
volatile uint8_t rx_state = TXRX_OK;

/**
 * Called when a byte is received from USART.
 */
ISR(USART_RX_vect)
{
	rx_buf[rx_in_i++] = UDR0;

	// Wrap to start
	if (rx_in_i == RX_BUF_SIZE) rx_in_i = 0;
	
	if (rx_in_i == rx_out_i) {
		// Overflow condition
		rx_state = TXRX_OVERFLOW;
	}
}

uint8_t serial_available(void) {
	// If it overflows, do not let reads to happen
	if (rx_state == TXRX_OVERFLOW) return 0;

	uint8_t diff = rx_in_i - rx_out_i;
	return (rx_in_i < rx_out_i) ? diff + RX_BUF_SIZE : diff;
}

void serial_RX_empty(void) {
	rx_out_i = rx_in_i;
	rx_state = TXRX_OK;
}

uint8_t serial_read(void) {
	uint8_t data = rx_buf[rx_out_i++];
	if (rx_out_i == RX_BUF_SIZE) rx_out_i = 0;
	return data;
}

void serial_ungetc(uint8_t x)
{
	// Done as single assignment this to avoid atomicity problem
	if (rx_out_i == 0) rx_out_i = RX_BUF_SIZE - 1;
	else rx_out_i--;

	rx_buf[rx_out_i] = x;

	if (rx_in_i == rx_out_i) {
		// Overflow condition
		rx_state = TXRX_OVERFLOW;
	}
}

uint8_t serial_read_blocking(void) {
	while(!serial_available()) {
		sleep_mode();
	}
	return serial_read();
}

#endif // AVR_ELO
