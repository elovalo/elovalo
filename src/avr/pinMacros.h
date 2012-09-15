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
 * pinMacros.h
 *
 *  Created on: 19.6.2012
 *      Author: Icchan
 */

#ifndef PINMACROS_H_
#define PINMACROS_H_

//********************************************************************
//          Macros for easy i/o pin access
//********************************************************************

#define BIT(p,b)                (b)

#define PORT(p,b)               (PORT ## p)
#define PIN(p,b)                (PIN ## p)
#define DDR(p,b)                (DDR ## p)

#define Set_Port_Bit(p,b)       ((p) |= _BV(b))
#define Clr_Port_Bit(p,b)       ((p) &= ~_BV(b))
#define Tgl_Port_Bit(p,b)       ((p) ^= _BV(b))

#define Get_Port_Bit(p,b)       (((p) & _BV(b)) != 0)

//user functions:
#define bit(io)                   BIT(io)
#define port(io)                PORT(io)

#define pin_high(io)            Set_Port_Bit(PORT(io),BIT(io))
#define pin_low(io)             Clr_Port_Bit(PORT(io),BIT(io))
#define pin_toggle(io)          Tgl_Port_Bit(PORT(io),BIT(io))

#define get_output(io)          Get_Port_Bit(PORT(io),BIT(io))
#define get_input(io)           Get_Port_Bit(PIN(io),BIT(io))

#define set_dir_in(io)          Clr_Port_Bit(DDR(io),BIT(io))
#define set_dir_out(io)         Set_Port_Bit(DDR(io),BIT(io))
#define dir_toggle(io)          Tgl_Port_Bit(DDR(io),BIT(io))

//********************************************************************
// define pins as:
// #define LED D,PD2
//
// write code as:
// pin_high(LED)
//
//********************************************************************

#endif /* PINMACROS_H_ */
