/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef CRON_H_
#define CRON_H_

#include <stdbool.h>
#include "clock.h"

typedef void(*action_t)(uint8_t);

enum event_kind {
	WEEKLY  = 0x00, // Weekly caledar
	ONETIME = 0x01, // One-time event
	EMPTY   = 0xfe, // Not set, go to next
	END     = 0xff  // End of schedule
	/* After adding types, remember to update validate_event() and
	   run_cron() */
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

extern const struct action_info cron_actions[]; 
extern const uint8_t cron_actions_len;

void run_cron(const time_t now);
void serial_hello(uint8_t x);

/**
 * Validate event contents (before accepting it from serial console).
 */
bool is_event_valid(struct event *e);

/**
 * Returns true if given action function pointer is allowed and false
 * if it is not an action.
 */
bool is_action_valid(action_t act);

#endif /* CRON_H_ */
