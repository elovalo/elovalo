#ifndef SCHEDULE_H_
#define SCHEDULE_H_

#include "timer.h"

typedef void(*action_t)(void);

enum event_kind {
	WEEKLY  = 0x00, // Weekly caledar
	ONETIME = 0x01, // One-time event
	EMPTY   = 0xfe, // Not set, go to next
	END     = 0xff  // End of schedule
};

struct event {
	enum event_kind kind;
	uint8_t act; // Action to run
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

// Define these things more portable way
#define CUBE_SHUTDOWN 0
#define CUBE_START 1
#define	SERIAL_HELLO 2


#endif /* SCHEDULE_H_ */
