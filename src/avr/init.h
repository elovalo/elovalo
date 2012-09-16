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
 * init.h
 *
 *  Created on: 24.5.2012
 *      Author: Icchan
 */

#ifndef INIT_H_
#define INIT_H_

/**
 * Sets up pins used by TLC5940.
 */
void init_tlc5940(void);

/**
 * Initializes pins used in SPI communication.
 */
void init_spi(void);

/**
 * Initializes BLANK Timer / Timer0
 */
void init_blank_timer(void);

/**
 * Initializes effect tick timer / Timer2
 */
void init_effect_timer(void);

void initUSART(void);

#define DEBUG_LED D,PD4

#endif /* INIT_H_ */
