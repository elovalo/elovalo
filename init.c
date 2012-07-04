/*
 * init.c
 *
 *  Created on: 24.5.2012
 *      Author: Icchan
 */

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/wdt.h>
#include <util/delay.h>
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
	MCUCR |= (0<<PUD); //Ensure we're using pull-ups if configured to be so...

	DDRB = 0x00; //Should be this way in reset, but to make sure...

	//TODO: Move these over to the respective modules...
	DDRB |= (1<<PB1)|(1<<PB2)|(1<<PB3)|(1<<PB5);
	//Set BLANK high (it doubles as !SS pin, thus it has to be an output to stay in master SPI mode)...
	PORTB |= (1<<PB2);

	DDRC = 0x00;

	DDRD = 0x00;
	DDRD |= (1<<PD2)|(1<<PD3)|(1<<PD4); //PD4 = debug led...
	PORTD |= (1<<PD3); //Put only VPRG to high on init...

}

void initSPI(){

	/* Set MOSI, !SS and SCK output*/
	DDRB = (1<<PB2)|(1<<PB3)|(1<<PB5);

	SPCR =
	(1<<SPIE) | //We want interrupts
	(1<<SPE) | 	//We want the SPI enabled
	(0<<DORD) | //We want the data to be shifted out MSB
	(1<<MSTR) | //We want the atmega to be a master
	(0<<CPOL) | //We want the leading edge to be rising
	(0<<CPHA) | //We want the leading edge to be sample
	(0<<SPR1) | (0<<SPR0) ; // sets the clock speed

	SPSR = (1<<SPI2X) ; //Doubles the speed of the SPI clock

}

void initUSART(){

	uint16_t ubrr = 103; //(F_CPU/(16UL*BAUD_RATE))-1;

	// disable all interrupts before configuration
	cli();

	// USART0 Baud Rate Register
	// set clock divider
	UBRR0H = (uint8_t)(ubrr >> 8);
	UBRR0L = (uint8_t)ubrr;

    // Set frame format to 8 data bits, no parity, 1 stop bit
    UCSR0C |= (1<<UCSZ01)|(1<<UCSZ00);
    //enable reception and RC complete interrupt
    UCSR0B |= (1<<RXEN0)|(1<<RXCIE0)|(1<<TXEN0);

}

//Initialize BLANK Timer Timer0
void initBLANKTimer(){
	//CTC with OCRA as TOP
	TCCR0A = (1 << WGM01);
	//Generate interrupt every 3x1024 (4096) clock cycles
	OCR0A = 3;
	// Enable Timer Compare match A interrupt
	TIMSK0 |= (1 << OCIE0A);
	//clk_io/1024 timer ON!
	TCCR0B = (1 << CS02) | (1 << CS00);
}
