/* c-basic-offset: 8; tab-width: 8; indent-tabs-mode: nil
 * vi: set shiftwidth=8 tabstop=8 expandtab:
 * :indentSize=8:tabSize=8:noTabs=true:
 */
/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

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
