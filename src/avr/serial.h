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

/* Functions for USART access */

#include <stdint.h>

#define TX_BUF_SIZE 16
#define RX_BUF_SIZE 64
#define TXRX_OK 0
#define TXRX_OVERFLOW 1

/* Receiver and transmitter states. TXRX_OK is normal value. */
extern volatile uint8_t tx_state;
extern volatile uint8_t rx_state;

/**
 * Returns the number of bytes available in receive buffer
 */
uint8_t serial_available(void);

/**
 * Empties receive buffer.
 */
void serial_RX_empty(void);

/**
 * Empties transmit buffer.
 * And clears the error condition
 */
void serial_TX_empty(void);

/**
 * Reads a byte from receive buffer. Does not check underrun
 * condition; the user is responsible to check serial_available()
 * before calling this.
 */
uint8_t serial_read(void);

/**
 * Put a byte back to serial buffer. Note the possibility of an
 * overflow. Check rx_state afterwards if you want to check we are in
 * a sane state.
 */
void serial_ungetc(uint8_t x);

/**
 * Returns free capacity in send buffer. That is how many bytes can be
 * transmitted at once.
 */
uint8_t serial_send_available(void);

/**
 * Send a byte to serial port. Does not check overflow condition; the
 * user is responsible to check serial_send_available() before calling
 * this. Do not use from interrupts (or manipulate atomic block)
 */
void serial_send_nonblocking(uint8_t data);

/**
 * Reads a byte from receive buffer. Waits in a busy wait loop for
 * more data if no data is already available.
 */
uint8_t serial_read_blocking(void);

/**
 * Send a byte to serial port. Checks overflow condition and blocks if
 * the buffer is full. Do not call from interrupts!
 */
void serial_send(uint8_t data);
