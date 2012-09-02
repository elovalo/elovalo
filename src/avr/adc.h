#ifndef ADC_H_
#define ADC_H_

void adc_init();
uint16_t adc_read(uint8_t ch);

uint16_t adc_get(uint8_t channel);

void adc_start(void);
void adc_stop(void);

#endif /* ADC_H_ */
