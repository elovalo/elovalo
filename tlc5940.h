/*
 * tlc5940.h
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */

#ifndef TLC5940_H_
#define TLC5940_H_

#define TLC5940 1 //Number of devices
#define GS_DATA_LENGHT (TLC5940*24)-1 //One layers data length
#define DC_DATA_LENGTH (TLC5940*12)-1 //Dot correction data lenght for one layer
#define USE_EEPROM_DC_DATA 1 //Use preprogrammed EEPROMdata in dot correction

//Define IO

/**TLC5940 pins*/
#define DCPRG 	D,PD2 /*Changes if the DC data is written to the EEPROM (low) or to the register (high)*/
#define VPRG 	D,PD3 //when high, TLC5940 is in DC mode, while low, in GS mode
#define XLAT 	B,PB1 //Data latch from shift register to the device registers
#define BLANK 	B,PB2 //BLANK every 4096 GScycles. Digital pin 10 ARDUINO

//SPI pins
#define MOSI 	B,PB3
#define MISO 	B,PB4
#define CLK 	B,PB5
#define SCLK 	CLK //For the TLC5940 abstraction... see flowchart.

//Global variables...

extern volatile uint8_t FirstCycle ;
extern volatile uint8_t GSdataCounter;
extern uint8_t DCdataCounter;
extern uint8_t c;

void initTLC5940();

void DCInputCycle();

void InitGScycle();

#endif /* TLC5940_H_ */
