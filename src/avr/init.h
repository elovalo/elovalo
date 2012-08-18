/*
 * init.h
 *
 *  Created on: 24.5.2012
 *      Author: Icchan
 */

#ifndef INIT_H_
#define INIT_H_

void init_tlc5940(void);
void init_spi(void);
void init_blank_timer(void);
void init_effect_timer(void);
void initUSART(void);

#define DEBUG_LED D,PD4

#endif /* INIT_H_ */
