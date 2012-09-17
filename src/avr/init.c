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

/*
 * init.c
 *
 * Some instructions for newbies:
 *
 * Logic 1 to DDRx pin is configured as an output pin.
 *
 * Logic 1 to PORTxn output when port configured as an output,
 * else activate pull up resistor.
 *
 * The DDxn bit in the DDRx Register selects the direction of this
 * pin. If DDxn is written logic one, Pxn is configured as an output
 * pin. If DDxn is written logic zero, Pxn is configured as an input
 * pin.
 *
 *  Created on: 24.5.2012
 *      Author: Icchan
 */

/* Timing accuracy with 8 data bits and 1 stop bit is about 4 percent:
   http://www.maximintegrated.com/app-notes/index.mvp/id/2141 */
#define BAUD 115200
#define BAUD_TOL 4

#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/setbaud.h>
#include "pinMacros.h"
#include "init.h"
#include "tlc5940.h"

void init_tlc5940(void)
{
	/* Set BLANK high (The pin used as BLANK is also !SS pin, thus
	 * it has to be an output to stay in master SPI mode). Setting
	 * state before direction to avoid blinking. And, because we
	 * don't use dot correction we may safely leave VPRG low on
	 * init. */
	PORTB |= (1<<PB2);
	DDRB |=
		(1<<PB1)| // XLAT: output
		(1<<PB2); // BLANK: output
	DDRD |=
		(1<<PD4); // Debug LED: output
}

void init_spi(void)
{
	DDRB |=
		(1<<PB2)| // !SS: output
		(1<<PB3)| // MOSI: output
		(1<<PB5); // SCK: output

	SPCR |=
		(1<<SPIE)| // Enable SPI interrupts
		(1<<SPE)|  // Enable SPI
		(1<<MSTR); // We want the to be a master

	/* Leaving SPI2X, SPR1, and SPR0 to initial zero value. That
	   runs SPI at f_osc / 4 = 4 MHz, when f_osc is 16 MHz */
}

void init_blank_timer(){
	/* We have 12 bit PWM cycle on TLC5940, prescaler of 1024, and
	 * TLC5940 clock divider of 4. So we need to have output
	 * compare value (interrupt interval) of 2^12/1024*4-1 */

	// CTC with OCRA as TOP
	TCCR0A = (1 << WGM01);
	// Interrupt generation interval is set by dimmer
	tlc5940_set_dimming(255);
	// Enable Timer Compare match A interrupt
	TIMSK0 |= (1 << OCIE0A);
	// Prescaler clk_io / 1024
	TCCR0B |= (1 << CS02) | (1 << CS00);
}

void init_effect_timer(){
	//CTC with OCRA as TOP
	TCCR2A |= (1 << WGM21);
	/* Generate interrupt every 125x1024 cycles, which gives clock
	   granularity of 8ms @ 16MHz */
	OCR2A = 124;
	// Enable Timer Compare match A interrupt
	TIMSK2 |= (1 << OCIE2A);
	// Prescaler clk_io / 1024
	TCCR2B |= (1 << CS22) | (1 << CS21) | (1 << CS20);
}

void initUSART(){

	DDRD |= (1<<PD1);

	/* See more information about the macro usage:
	 * http://www.nongnu.org/avr-libc/user-manual/group__util__setbaud.html
	 */
	UBRR0H = UBRRH_VALUE;
	UBRR0L = UBRRL_VALUE;
#if USE_2X
	UCSR0A |= (1 << U2X0);
#else
	UCSR0A &= ~(1 << U2X0);
#endif

	// Set frame format to 8 data bits, no parity, 1 stop bit
	UCSR0C |= (1<<UCSZ01)|(1<<UCSZ00);

	UCSR0B |= (1<<TXEN0);  // Transmit enable
	UCSR0B |= (1<<RXEN0);  // Receive enable
	UCSR0B |= (1<<RXCIE0); // Receive ready interrupt
	UCSR0B |= (1<<TXCIE0); // transmit ready interrupt

}
