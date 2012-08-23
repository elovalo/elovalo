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

register uint8_t layer_bytes_left asm ("r4");
register uint8_t *send_ptr asm ("r2");
volatile uint8_t may_flip = 0;

#define NL "\n\t"

/* SPI transmit interrupt vector SPIF is cleared when entering this
 * interrupt vector. This is called when byte transmission is
 * finished.
 */
ISR(SPI_STC_vect, ISR_NAKED)
{
	asm volatile(
		"push    r1"        NL
		"push    r0"        NL
		"in      r0, 0x3f"  NL
		"push    r0"        NL
		"eor     r1, r1"    NL
		"push    r24"       NL
		"push    r30"       NL
		"push    r31"       NL
		"and     r4, r4"    NL
		"breq    .+14"      NL
		"dec     r4"        NL
		"movw    r30, r2"   NL
		"ld      r24, Z"    NL
		"out     0x2e, r24" NL
		"ldi     r24, 0xFF" NL
		"sub     r2, r24"   NL
		"sbc     r3, r24"   NL
		"pop     r31"       NL
		"pop     r30"       NL
		"pop     r24"       NL
		"pop     r0"        NL
		"out     0x3f, r0"  NL
		"pop     r0"        NL
		"pop     r1"        NL
		"reti"              NL
		);
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

	// Send first byte
	SPDR = ~layer;

	// Main screen turn on and start PWM timers on TLC5940
	pin_low(BLANK);

	if (layer != 0x80) {
		// Advance layer
		layer <<= 1;
	} else {
		// Prepare drawing first layer
		layer=0x01;
		
		// If we have new buffer, flip to it
		if (may_flip) {
			gs_buf_swap();
			may_flip = 0;
		}

		// Roll send_ptr back to start of buffer
		send_ptr = gs_buf_front;
	}

	// Set up byte counter for SPI interrupt
	layer_bytes_left = BYTES_PER_LAYER;
}
