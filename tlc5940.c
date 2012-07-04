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

uint8_t FirstCycle = 0; //Is this the first cycle after DCinputCycle()...
uint8_t GSdataCounter = 0; //Counter to index of GSdata[] array, has to be volatile since it's modified at ISR
uint8_t DCdataCounter = 0; //Counter to index of DCdata[] array
uint8_t isAfterFlip = 0;

//Sets all the signals to their expected values and initiates the dot correction cycle...
void initTLC5940(){
	pin_low(XLAT);
	pin_low(DCPRG);
	pin_high(VPRG);
	pin_high(BLANK);
	pin_low(SCLK);
	DCInputCycle();
}

/*
 * Send new Dot Correction data for TLC5940
 * Skipped if USE_EEPROM_DATA is set...
 */
void DCInputCycle(){

	if(USE_EEPROM_DC_DATA){
		pin_low(DCPRG);
	}
	else{
		pin_high(DCPRG); //Dot correction register programming on
		pin_high(VPRG); //Set dot correction data input mode on

		//Send dot correction data to the SPI bus...
		for(DCdataCounter = 0; DCdataCounter < DC_DATA_LENGTH; DCdataCounter++){
			/* Start transmission */
			SPDR = 0xff; //Send byte
			//TODO: allow usage of dot correction data array...
			/* Wait for transmission complete */
			while(!(SPSR & (1<<SPIF)));
		}
		//Pulse XLAT to latch sent data...
		pin_high(XLAT);
		pin_low(XLAT);
		//TODO: Support EEPROM writing with 22V VPRG signal?

	}
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

	//TODO: TEST if this is ok!
	//TCCR0B = (1 << CS02) | (1 << CS00); //Enable timer!

	pin_low(BLANK);

	SPDR = FrontBuffer[GSdataCounter];
	//TCNT0 =0x00; //Reset timer....

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
	else{
		GSdataCounter=0;
		//TODO: access SPDR to clear SPIF flag. (it's supposed to be cleared when entering ISR)
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

	if(c>=255){
	// flipitiflip bufferille
		Midbuffer = FrontBuffer;
		FrontBuffer = BackBuffer;
		BackBuffer = Midbuffer;

		c=0;
		pin_toggle(DEBUG_LED);
		isAfterFlip = 1;
	}

   // Start new transfer....
   InitGScycle();
}

