#include <stdint.h>
#include <string.h>
#include "effects/lib/utils.h"
#include "effects.h"
#include "effect_utils.h"

const effect_t *find_effect(const char *name) {
	for(int i = 0; i < effects_len; i++) {
		if(strcmp(effects[i].name, name) == 0) {
			return &effects[i];
		}
	}

	// TODO: deal with this situation at top level
	return &effects[effects_len];
}
