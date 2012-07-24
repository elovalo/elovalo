/*
 * main.h
 *
 *  Created on: 2.4.2010
 *      Author: Icchan
 */
#ifndef MAIN_H_
#define MAIN_H_

//Global variables...
volatile uint16_t c; //testing variable...

//Global pointer...
extern uint8_t *FrontBuffer;
extern uint8_t *BackBuffer;

//TODO: Enable these functions and implement them properly for generic SPI communication if necessary.
//void SPI_Transfer(uint8_t cData);
//void SPI_Transfer_TLC5940(uint8_t *FrontBuffer);
int main(void);

void clearArray(volatile uint8_t *arr, uint8_t);

void serial_send(uint8_t data);
uint8_t serial_available(void);
uint8_t serial_send_available(void);
void serial_RX_empty(void);
void serial_TX_empty(void);
uint8_t serial_read(void);

void processCommand();
void stateMachine();
void animSnake();

#endif /* MAIN_H_ */
