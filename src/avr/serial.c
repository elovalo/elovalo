/* Functions for USART access */

#include <avr/interrupt.h>
#include "serial.h"

// RX ring buffer
uint8_t rx_buf[RX_BUF_SIZE];
volatile uint8_t rx_in_i = 0; // Set by USART_RX_vect
uint8_t rx_out_i = 0;

// TX ring buffer
uint8_t tx_buf[TX_BUF_SIZE];
uint8_t tx_in_i = 0;
volatile uint8_t tx_out_i = 0; // Set by USART_TX_vect

// Receiver and transmitter states
volatile uint8_t tx_state = TXRX_OK;
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

/**
 * Called when USART has finished transmit.
 */
ISR(USART_TX_vect)
{
	// If no data in buffer, then we just bail out.
	if (tx_in_i == tx_out_i) return;

	// Send the byte and wrap to start if needed
	UDR0 = tx_buf[tx_out_i++];
	if (tx_out_i == TX_BUF_SIZE) tx_out_i = 0;
}

/**
 * Returns the number of bytes available in receive buffer
 */
uint8_t serial_available(void) {
	uint8_t diff = rx_in_i - rx_out_i;
	return (rx_in_i < rx_out_i) ? diff + RX_BUF_SIZE : diff;
}

/**
 * Empties receive buffer.
 */
void serial_RX_empty(void) {
	rx_out_i = rx_in_i;
}

/**
 * Empties transmit buffer.
 * And clears the error condition
 */
void serial_TX_empty(void) {
	tx_in_i = tx_out_i;
	tx_state = TXRX_OK;
}

/**
 * Reads a byte from receive buffer. Does not check underrun
 * condition; the user is responsible to check serial_available()
 * before calling this.
 */
uint8_t serial_read(void) {
	uint8_t data = rx_buf[rx_out_i++];
	if (rx_out_i == RX_BUF_SIZE) rx_out_i = 0;
	return data;
}

/**
 * Put a byte back to serial buffer. Note the possibility of an
 * overflow. Check rx_state afterwards if you want to check we are in
 * a sane state.
 */
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

/**
 * Returns free capacity in send buffer. That is how many bytes can be
 * transmitted at once.
 */
uint8_t serial_send_available(void) {
	uint8_t diff = tx_out_i - tx_in_i - 1;
	return (tx_out_i < tx_in_i + 1) ? diff + TX_BUF_SIZE : diff;
}

/**
 * Send a byte to serial port. Does not check overflow condition; the
 * user is responsible to check serial_send_available() before calling
 * this.
 */
void serial_send(uint8_t data)
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
		if (tx_in_i == RX_BUF_SIZE) tx_in_i = 0;

		if (tx_in_i == tx_out_i) {
			// Overflow condition
			tx_state = TXRX_OVERFLOW;
		}
	}
}

/**
 * Reads a byte from receive buffer. Waits in a busy wait loop for
 * more data if no data is already available.
 */
uint8_t serial_read_blocking(void) {
	while(!serial_available());
	return serial_read();
}
