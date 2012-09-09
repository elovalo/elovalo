#ifndef CRON_H_
#define CRON_H_

#include "clock.h"

typedef void(*action_t)(uint8_t);

enum event_kind {
	WEEKLY  = 0x00, // Weekly caledar
	ONETIME = 0x01, // One-time event
	EMPTY   = 0xfe, // Not set, go to next
	END     = 0xff  // End of schedule
};

struct event {
	enum event_kind kind;
	action_t act; // Function to run
	uint8_t arg;  // Argument for the action
	union {
		struct {
			uint8_t weekdays; // Thursday is LSB
			int16_t minutes;  // Minutes from midnight, UTC
		} weekly;
		struct {
			time_t ts;   // Timestamp of execution
		} onetime;
	} u;
};

/* This structure holds information about the cron actions */
struct action_info {
	action_t act;         // Action function pointer
	const char *act_name; // Name for action
	const char *arg_name; // Name of the argument. NULL if not used.
};

void run_cron_check(const time_t now);
void serial_hello(uint8_t x);

#endif /* CRON_H_ */
