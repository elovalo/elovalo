/*
 * tlc5940.h
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */

#ifndef TLC5940_H_
#define TLC5940_H_

#define TLC5940 1 //Number of devices
#define USE_EEPROM_DC_DATA 1

void init();

void DCInputCycle();

void grayscaleCycle();

void PWMcycle();


#endif /* TLC5940_H_ */
