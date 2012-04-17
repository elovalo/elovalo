/*
 * main.h
 *
 *  Created on: 2.4.2010
 *      Author: Icchan
 */
#include <avr/io.h>
#include <inttypes.h>

#ifndef MAIN_H_
#define MAIN_H_

void initPorts(void);
void initYM2612(void);

void readWriteFromYM2612(void);

int main(void);

#endif /* MAIN_H_ */
