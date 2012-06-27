/*
 * init.h
 *
 *  Created on: 24.5.2012
 *      Author: Icchan
 */

#ifndef INIT_H_
#define INIT_H_

void initPorts(void);
void initSPI(void);
void initUSART(void);

#define DEBUG_LED D,PD4

#endif /* INIT_H_ */
