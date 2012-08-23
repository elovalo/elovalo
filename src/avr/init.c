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

#include <avr/interrupt.h>
#include <avr/io.h>
#include "pinMacros.h"
#include "init.h"

/**
 * Sets up pins used by TLC5940.
 */
void init_tlc5940(void)
{
	/* Set BLANK high (The pin used as BLANK is also !SS pin, thus
	 * it has to be an output to stay in master SPI mode). Setting
	 * state before direction to avoid blinking. And, because we
	 * don't use dot correction we may safely leave VPRG low on
	 * init. */
	PORTB |= (1<<PB2);
	DDRB |=
		(1<<PB1); // XLAT: output

	DDRD |=
		(1<<PD2)| // DCPRG: output
		(1<<PD3)| // VPRG: output
		(1<<PD4)| // Debug LED: output
		(1<<PD7); // BLANK: output
}

/**
 * Initializes pins used in SPI communication.
 */
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

/**
 * Initializes BLANK Timer / Timer0
 */
void init_blank_timer(){
	// CTC with OCRA as TOP
	TCCR0A = (1 << WGM01);
	// Generate interrupt every 8x1024 clock cycles
	OCR0A = 7;
	// Enable Timer Compare match A interrupt
	TIMSK0 |= (1 << OCIE0A);
	// Prescaler clk_io / 1024
	TCCR0B |= (1 << CS02) | (1 << CS00);
}

/**
 * Initializes effect tick timer / Timer2
 */
void init_effect_timer(){
	//CTC with OCRA as TOP
	TCCR2A |= (1 << WGM21);
	// Generate interrupt every 156x1024 cycles (9.984ms)
	OCR2A = 156;
	// Enable Timer Compare match A interrupt
	TIMSK2 |= (1 << OCIE2A);
	// Prescaler clk_io / 1024
	TCCR2B |= (1 << CS22) | (1 << CS21) | (1 << CS20);
}

void initUSART(){

	uint16_t ubrr = 103; //(F_CPU/(16UL*BAUD_RATE))-1;

	DDRD |= (1<<PD1);

	// USART0 Baud Rate Register
	// set clock divider
	UBRR0H = (uint8_t)(ubrr >> 8);
	UBRR0L = (uint8_t)ubrr;

	// Set frame format to 8 data bits, no parity, 1 stop bit
	UCSR0C |= (1<<UCSZ01)|(1<<UCSZ00);

	UCSR0B |= (1<<TXEN0);  // Transmit enable
	UCSR0B |= (1<<RXEN0);  // Receive enable
	UCSR0B |= (1<<RXCIE0); // Receive ready interrupt
	UCSR0B |= (1<<TXCIE0); // transmit ready interrupt

}
