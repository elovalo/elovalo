/*
 * tlc5940.c
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

volatile uint8_t FirstCycle = 0; //Is this the first cycle after DCinputCycle()...
volatile uint8_t GSdataCounter = 0; //Counter to index of GSdata[] array, has to be volatile since it's modified at ISR
volatile uint8_t isAfterFlip = 0;
volatile uint8_t layer=0x01;
volatile uint16_t c; //testing variable...

//Sets all the signals to their expected values and initiates the dot correction cycle...
void initTLC5940(){
	pin_low(XLAT);
	pin_low(DCPRG);
	pin_high(VPRG);
	pin_high(BLANK);
	pin_low(SCLK);
	pin_low(DCPRG); /* Dot correction is read from EEPROM which
			 * effectively turns it off */
}

/*
 * Initiates the Grays Scale data transfer by putting first byte to the SPDR
 * Consecutive bytes are sent by an ISR: SPI_STC_vect
 */
void InitGScycle(){

	//Check if DC cycle was run before this...
	if(get_output(VPRG)){
		pin_low(VPRG);
		FirstCycle = 1;
	}

	pin_low(BLANK);

	SPDR = FrontBuffer[GSdataCounter];

}

/*SPI transmit interrupt vector
 * SPIF is cleared when entering this interrupt vector...
 */
ISR(SPI_STC_vect)
{
	if(GSdataCounter < GS_DATA_LENGHT){
		GSdataCounter++;
		SPDR = FrontBuffer[GSdataCounter];
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
	pin_low(XLAT);

	if(FirstCycle){
		pin_high(SCLK);
		pin_low(SCLK);
		FirstCycle = 0;
	}



	if(layer==0x80){
		layer=0x01;
	}
	else{
		layer=layer<<1;
	}

	if(c>=50){
		//Flip buffers...
		uint8_t *tmp = FrontBuffer;
		FrontBuffer = BackBuffer;
		BackBuffer = tmp;

		c=0;
		isAfterFlip = 1;
	}

   // Start new transfer....
	pin_low(BLANK);

	if(isAfterFlip){
	SPDR = FrontBuffer[GSdataCounter];
	}

}

