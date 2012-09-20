/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
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

/**
 * JSON exporter for non-embedded use.
 *
 * Usage: ./exporter <effect> <length in ms>
 */

#include <assert.h>
#include <jansson.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "../effects/lib/utils.h"
#include "../effect_utils.h"
#include "../cube.h"

void export_effect(const effect_t *effect, int length, const char *sensor_path, const char *data);

int main(int argc, char **argv) {
	mkdir("exports", S_IRWXU);

	// TODO: figure out what should happen if an effect is not found by name
	if(argc < 3) printf("Missing effect and length arguments!\n");
	else if(argc == 3) export_effect(find_effect(argv[1]), atoi(argv[2]), "", "");
	else if(argc == 4) export_effect(find_effect(argv[1]), atoi(argv[2]), argv[3], "");
	else if(argc == 5) export_effect(find_effect(argv[1]), atoi(argv[2]), argv[3], argv[4]);
}

void export_effect(const effect_t *effect, int length, const char *sensor_path, const char *data) {
	const int size = 50;
	char filename[size];

	// XXX: figure out how to fetch array size
	int distance1[10000];
	int distance2[10000];
	int ambient_light[10000];
	int sound_pressure[10000];

	/* Attach custom data */
	custom_data = data;

	/* Parse sensor json */
	if(strlen(sensor_path) > 0) {
		json_error_t error;
		json_t *root = json_load_file(sensor_path, 0, &error);

		if(!root) {
			fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
		}
		else if(!json_is_object(root)) {
			fprintf(stderr, "error: root is not an object\n");
		}
		else {
			json_unpack(root, "{s:[i], s:[i], s:[i], s:[i]}",
				"distance1", distance1,
				"distance2", distance2,
				"ambient_light", ambient_light,
				"sound_pressure", sound_pressure
			);
		}
	}

	/* Increment frame counter always by 2 centiseconds
	   to simulate slow drawing. */
	const uint16_t drawing_time = 2;
	
	int bytes = snprintf(filename, size, "exports/%s.json", effect->name);
	assert(bytes <= size);

	printf("Exporting %f seconds of %s to file %s\n",
		(double)length/1000,
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

	for (ticks=0; ticks < length * 10; ticks += drawing_time) {
 		/* TODO fill in sensor data struct declared at
		   src/effects/lib/utils.h */

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
