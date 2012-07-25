/*
 * main.h
 *
 *  Created on: 2.4.2010
 *      Author: Icchan
 */
#ifndef MAIN_H_
#define MAIN_H_

//TODO: Enable these functions and implement them properly for generic SPI communication if necessary.
//void SPI_Transfer(uint8_t cData);
//void SPI_Transfer_TLC5940(uint8_t *FrontBuffer);
int main(void);

void clearArray(volatile uint8_t *arr, uint8_t);

void processCommand();
void stateMachine();
void animSnake();

#endif /* MAIN_H_ */
