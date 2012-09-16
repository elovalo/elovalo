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
#define XLAT 	B,PB1 // Data latch from shift register to the device registers
#define BLANK 	B,PB2 // BLANK pin.

// SPI pins
#define MOSI 	B,PB3
#define MISO 	B,PB4
#define CLK 	B,PB5
#define SCLK 	CLK //For the TLC5940 abstraction... see flowchart.

// Global variables
extern volatile uint8_t may_flip;

/**
 * Set global dimming of the LED cube. Possible values range from 0 to
 * 255. It's performed by tuning BLANK interval which may lead to
 * flickering when using low intensities. Also, there is certain low
 * limit for intensity. Use the functions in powersave.h if you need
 * to turn the cube completely off. */
void tlc5940_set_dimming(uint8_t x);

#endif /* TLC5940_H_ */
