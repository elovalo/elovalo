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

void serial_send_nonblocking(uint8_t data);
uint8_t serial_available(void);
uint8_t serial_send_available(void);
void serial_RX_empty(void);
void serial_TX_empty(void);
uint8_t serial_read(void);
uint8_t serial_read_blocking(void);
void serial_ungetc(uint8_t x);
void serial_send(uint8_t data);
