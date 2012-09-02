#ifndef HCSR04_H_
#define HCSR04_H_

void hcsr04_init(void);
void hcsr04_send_pulse(void);
uint16_t hcsr04_get_pulse_length(void);

#endif /* HCSR04_H_ */
