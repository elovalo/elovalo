/*
 * tlc5940.c
 *
 *  Created on: 12.6.2012
 *      Author: Icchan
 */
#include "tlc5940.h"
#include <avr/io.h>

//void init(){
//	DCInputCycle();
//}
//
//void DCInputCycle(){
//
//	uint16_t DataCounter = 0;
//
//	if(USE_EEPROM_DC_DATA){
//		//DCPRG = high; //EEPROM Programming on
//		//VPRG = high; //Set the dot correction input on
//
//		for(;DataCounter > TLC5940 * 96 -1;DataCounter++){
//			//send dot correction values to SPI
//			//DCData[DataCounter];
//		}
//		//Pulse XLAT
//
//	}
//
//}
//
//void grayscaleCycle(){
//
//	if(VPRG = high){
//		VPRG=low;
//		FirstCycle = 1;
//	}
//
//	GSCLKCounter = 0;
//	GS_DataCounter =0;
//	BLANK = low;
//
//	if(GSCLK <= 4095){
//
//		for(;GS_DataCounter > TLC5940*192-1;GSCLK++ ){
//			//Send GS data to SPI
//			//GS_Data[GS_DataCounter]
//			//GS_DataCounter++
//			//PulseGSCLK
//
//		}
//
//	}
//
//	BLANK = high;
//	pulse XLAT;
//
//	if(FisrtCycle = 1){
//		SCLK = 0;
//		SCLK = 1;
//		FirstCycle = 0;
//	}
//
//
//}
//
//void PWMcycle(){
//
//}
