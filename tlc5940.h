/*
 * tlc5940.h
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */

#ifndef TLC5940_H_
#define TLC5940_H_

#define TLC5940 1 //Number of devices
#define GS_DATA_LENGHT TLC5940*192/8-1 //One layers data length
#define DC_DATA_LENGTH TLC5940*96/8-1 //Dot correction data lenght for one layer
#define USE_EEPROM_DC_DATA 1 //Use preprogrammed EEPROMdata in dot correction

//Define IO
#define XLAT B,PB1
#define DCPRG D,PD2
#define VPRG D,PD3
#define BLANK B,PB2
#define CLK B,PB5
#define SCLK CLK



void init();

void DCInputCycle();

void InitGScycle();

void PWMcycle();

#endif /* TLC5940_H_ */
