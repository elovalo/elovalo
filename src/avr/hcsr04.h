#ifndef HCSR04_H_
#define HCSR04_H_

/**
 * Device driver interface functions for HC-SR04 ultrasound module
 */

/**
 * Initializes the driver state machine
 */
void hcsr04_init(void);

/**
 * Starts a new measurement cycle.
 * Returns 1 on success, 0 on failure
 */
int hcsr04_send_pulse(void);

/**
 * Returns the result value of the latest measurement cycle,
 * or HCSR04_MEAS_FAIL on failure.
 */
#define HCSR04_MEAS_FAIL 0xFFFF
uint16_t hcsr04_get_pulse_length(void);

#endif /* HCSR04_H_ */
