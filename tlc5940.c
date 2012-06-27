/*
 * tlc5940.c
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */
#include <avr/io.h>
#include "tlc5940.h"
#include "pinMacros.h"
#include "main.h"

extern uint8_t *FrontBuffer;
extern uint8_t FirstCycle;
extern uint8_t GSdataCounter;

uint8_t DCdataCounter = 0;

void init(){
	DCInputCycle();
}

void DCInputCycle(){

	if(USE_EEPROM_DC_DATA){
		pin_low(DCPRG);
	}

	else{
		pin_high(DCPRG); //EEPROM Programming on
		pin_high(VPRG); //Set the dot correction input on


		for(DCdataCounter = 0;DCdataCounter >= DC_DATA_LENGTH;DCdataCounter++){
			/* Start transmission */
			SPDR = 0xff; //Send byte
			/* Wait fortransmission complete */
			while(!(SPSR & (1<<SPIF)));
		}
		//Pulse XLAT
		pin_high(XLAT);
		pin_low(XLAT);
		//TODO: Support EEPROM?
	}
}

void InitGScycle(){

	if(get_output(VPRG)){
		pin_low(VPRG);
		FirstCycle = 1;
	}

	pin_low(BLANK);

	SPDR = FrontBuffer[GSdataCounter];
}
