#ifndef HCSR04_H_
#define HCSR04_H_

//return value for hcsr04_get_pulse_length() when measurement failed
#define HCSR04_MEAS_FAIL 0xFFFF

//initializes the driver state machine
void hcsr04_init(void);

//returns 1 on success, 0 on failure
int hcsr04_send_pulse(void);

//returns the pulse length, or HCSR04_MEAS_FAIL on failure
uint16_t hcsr04_get_pulse_length(void);

#endif /* HCSR04_H_ */
