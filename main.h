/*
 * main.h
 *
 *  Created on: 2.4.2010
 *      Author: Icchan
 */
#ifndef MAIN_H_
#define MAIN_H_

//Global variables...
volatile uint8_t c; //testing variable...

//Global pointer...
extern volatile uint8_t *FrontBuffer;
extern volatile uint8_t *BackBuffer;
extern volatile uint8_t *Midbuffer;

//TODO: Enable these functions and implement them properly for generic SPI communication if necessary.
//void SPI_Transfer(uint8_t cData);
//void SPI_Transfer_TLC5940(uint8_t *FrontBuffer);
int main(void);
void zeroArray(volatile uint8_t*, uint8_t);

#endif /* MAIN_H_ */
