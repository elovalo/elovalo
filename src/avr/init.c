/*
 * init.c
 *
 *  Created on: 24.5.2012
 *      Author: Icchan
 */

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/wdt.h>
#include "pinMacros.h"
#include "init.h"

void disableWDT(){
	wdt_disable(); //Mostly for debugging...
}

/*
 * Logic 1 to DDRx pin is configured as an output pin.
 * Logic 1 to PORTxn output when port configured as an output, else activate pull up resistor.
 *
 * The DDxn bit in the DDRx Register selects the direction of this pin. If DDxn is written logic one,
 * Pxn is configured as an output pin. If DDxn is written logic zero, Pxn is configured as an input pin.
 */
void initPorts(){

	//TODO: Remove unnecessary initialization and trust the defaults from the datasheet after debugging...
	//MCUCR = (0<<PUD); //Ensure we're using pull-ups if configured to be so...

	//DDRB = 0x00; //Should be this way after reset, but to make sure...

	//TODO: Move these over to the respective modules...

	//Set BLANK high (it doubles as !SS pin, thus it has to be an output to stay in master SPI mode)...
	DDRB |= (1<<PB1)|(1<<PB2)|(1<<PB3)|(1<<PB5);
	PORTB |= (1<<PB2);

	DDRD |= (1<<PD2)|(1<<PD3)|(1<<PD4); //PD4 = debug led...
	PORTD |= (1<<PD3); //Put only VPRG to high on init...

}

void initSPI(){

	/* Set MOSI, !SS and SCK output*/
	DDRB |= (1<<PB2)|(1<<PB3)|(1<<PB5);

	SPCR |=
	(1<<SPIE) | //We want interrupts
	(1<<SPE) | 	//We want the SPI enabled
	(1<<MSTR); //We want the atmega to be a master

	SPSR |= (1<<SPI2X) ; //Doubles the speed of the SPI clock

}

/**
 * Initializes BLANK Timer / Timer0
 */
void init_blank_timer(){
	// CTC with OCRA as TOP
	TCCR0A = (1 << WGM01);
	// Generate interrupt every 4x1024 (4096) clock cycles
	OCR0A = 3;
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


