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

volatile uint8_t GSdataCounter = 0; //Counter to index of GSdata[] array, has to be volatile since it's modified at ISR
volatile uint8_t isAfterFlip = 0;
volatile uint8_t layer=0x01;
volatile uint16_t c; //testing variable...

/*SPI transmit interrupt vector
 * SPIF is cleared when entering this interrupt vector...
 */
ISR(SPI_STC_vect)
{
	if(GSdataCounter < GS_DATA_LENGHT){
		GSdataCounter++;
		SPDR = gs_buf_front[GSdataCounter];
	}
	else if(GSdataCounter==GS_DATA_LENGHT){
		SPDR=layer;
		GSdataCounter++;
	}

	else{
		GSdataCounter=0;
	}
}

/*
 * BLANK timer interrupt Timer0
 * Interrupt if TCNT0 = OCR0A
 */
ISR(TIMER0_COMPA_vect)
{
	c++;
	pin_high(BLANK);
	pin_high(XLAT);
	pin_low(XLAT); // "Activate" data

	if(layer==0x80){
		layer=0x01;
	}
	else{
		layer=layer<<1;
	}

	if(c>=50){
		gs_buf_swap();
		c=0;
		isAfterFlip = 1;
	}

	// Start new transfer....
	pin_low(BLANK); // Start PWM timers on TLC5940

	if(isAfterFlip){
	SPDR = gs_buf_front[GSdataCounter];
	}

}

