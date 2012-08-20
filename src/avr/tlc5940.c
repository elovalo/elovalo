/*
 * tlc5940.c
 *
 * Ensure that a) all SPI and TLC5940 pins are configured as outputs
 * and b) BLANK is HIGH and other output pins of TLC5940 are LOW after
 * init. This module doesn't do any configuration of pins.
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */
#include <avr/io.h>
#include <avr/interrupt.h>
#include "tlc5940.h"
#include "pinMacros.h"
#include "init.h"
#include "main.h"
#include "../cube.h"

uint8_t spi_bytes_left;
uint8_t *send_ptr;
volatile uint8_t may_flip = 0;

/* SPI transmit interrupt vector SPIF is cleared when entering this
 * interrupt vector. This is called when byte transmission is
 * finished.
 */
ISR(SPI_STC_vect)
{
	if(--spi_bytes_left) SPDR = *send_ptr++;
}

/*
 * BLANK timer interrupt Timer0
 * Interrupt if TCNT0 = OCR0A
 */
ISR(TIMER0_COMPA_vect)
{
	static uint8_t layer = 0x01;

	// Main screen turn off
	pin_high(BLANK);

	// "Activate" previously uploaded data
	pin_high(XLAT);
	pin_low(XLAT);

	// Main screen turn on and start PWM timers on TLC5940
	pin_low(BLANK);

	if(layer==0x80) {
		// Prepare drawing first layer
		layer=0x01;
		
		// If we have new buffer, flip to it
		if (may_flip) {
			gs_buf_swap();
			send_ptr = gs_buf_front;
			may_flip = 0;
		}
	}
	else {
		// Advance layer
		layer=layer<<1;
	}
	
	// Set up byte counter for SPI interrupt
	spi_bytes_left = BYTES_PER_LAYER + 1;
	
	// Send first byte
	SPDR = layer;
}
