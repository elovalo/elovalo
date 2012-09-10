/**
 * JSON exporter for non-embedded use.
 *
 * TODO: might be nicer to pass time in ms
 * TODO:./exporter <effect> 1000 causes double to overflow? nasty print
 *
 * Usage: ./exporter <effect> <length in cs>
 */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "../effects/lib/utils.h"
#include "../effects.h"
#include "../cube.h"

void export_effect(const effect_t *effect, char length);
const effect_t *find_effect(const char *name);

int main(int argc, char **argv) {
	mkdir("exports", S_IRWXU);

	if(argc == 3) export_effect(find_effect(argv[1]), atoi(argv[2]));
	else printf("Missing effect and length arguments!\n");
}

// might want to move this elsewhere so playlist player may use this
const effect_t *find_effect(const char *name) {
	for(int i = 0; i < effects_len; i++) {
		if(strcmp(effects[i].name, name) == 0) {
			return &effects[i];
		}
	}

	// TODO: deal with this situation at top level
	return &effects[effects_len];
}

void export_effect(const effect_t *effect, char length) {
	const int size = 50;
	char filename[size];

	/* Increment frame counter always by 2 centiseconds
	   to simulate slow drawing. */
	const uint16_t drawing_time = 2;
	
	int bytes = snprintf(filename, size, "exports/%s.json", effect->name);
	assert(bytes <= size);

	printf("Exporting %f seconds of %s to file %s\n",
	       (double)length/100,
	       effect->name,
	       filename);

	FILE *f = fopen(filename,"w");
	if (f == NULL) {
		fprintf(stderr,"Unable to write to effect.dump\n");
		return;
	}

	/* If not flipping buffers, front must equal to back to
	 * support simultaneous drawing of front buffer */
	uint8_t *old_front = NULL;
	if (!effect->flip_buffers || effect->draw == NULL) {
		old_front = gs_buf_front;
		gs_buf_front = gs_buf_back;
	}

	if (effect->init != NULL) {
		effect->init();
		gs_buf_swap(); /* Flip to bring initialized data
				* accessible by get_led() */
	}

	// Draw the frames
	fputs("{\"fps\":25,\"geometry\":[8,8,8],\"frames\":[[",f); // TODO handle errors

	for (ticks=0; ticks < length; ticks += drawing_time) {
		if(effect->draw != NULL) effect->draw();

		// Flip buffers to better simulate the environment
		gs_buf_swap();

		// Export stuff
		for (int j=0; j<768; j+=3) {
			uint16_t fst =
				gs_buf_front[j] << 4 |
				gs_buf_front[j+1] >> 4;
			uint16_t snd =
				((gs_buf_front[j+1] & 0x0f) << 8) |
				gs_buf_front[j+2];

			fprintf(f,"%f,%f,",(float)fst/4095,(float)snd/4095);
		}

		// Unwind last comma
		fseek(f,-1,SEEK_CUR); // TODO handle errors
		fputs("],[",f); // TODO handle errors
	}

	// Return buffers back to original
	if (!effect->flip_buffers) gs_buf_front = old_front;

	fseek(f,-2,SEEK_CUR); // TODO handle errors
	fputs("]}\n",f); // TODO handle errors
	fclose(f); // TODO handle errors
}
