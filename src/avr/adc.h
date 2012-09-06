#ifndef ADC_H_
#define ADC_H_

/**
 * Device driver interface functions for ATmega328P ADC
 * See doc8161.pdf Rev. 8161D – 10/09, ch 23
 */

#if 0
/**
 * Initialize ADC channels
 * Required only if adc_read() is used.
 */
void adc_init();

/**
 * Starts a conversion cycle and waits for it to complete
 * For testing purposes only.
 */
uint16_t adc_read(uint8_t ch);
#endif

/**
 * Returns the latest conversion result for a given channel
 */
uint16_t adc_get(uint8_t channel);

/**
 * Starts a continuous conversion.
 * Results can be read with adc_get().
 */
void adc_start(void);

/**
 * Stops the continuous conversion which was started with adc_start().
 */
void adc_stop(void);

#endif /* ADC_H_ */
