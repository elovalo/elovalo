/*
 * tlc5940.h
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */

#ifndef TLC5940_H_
#define TLC5940_H_

// TLC5940 pins
#define DCPRG 	D,PD2 /* Changes if the DC data is written to the
		       * EEPROM (low) or to the register (high)*/
#define VPRG 	D,PD3 //when high, TLC5940 is in DC mode, while low, in GS mode
#define XLAT 	B,PB1 //Data latch from shift register to the device registers
#define BLANK 	D,PD7 // BLANK pin. Digital pin 7 on Arduino

// SPI pins
#define MOSI 	B,PB3
#define MISO 	B,PB4
#define CLK 	B,PB5
#define SCLK 	CLK //For the TLC5940 abstraction... see flowchart.

// Global variables
extern volatile uint8_t may_flip;

#endif /* TLC5940_H_ */
