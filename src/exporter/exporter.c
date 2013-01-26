/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * JSON exporter for non-embedded use.
 */

#include <assert.h>
#include <jansson.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "../effects/lib/utils.h"
#include "../common/effect_utils.h"
#include "../common/cube.h"

void export_effect(const effect_t *effect, double length, const char *sensor_path,
		   const char *data, bool binary);

int main(int argc, char **argv) {
	bool binary = false;
	const char* prog = argv[0];

	mkdir("exports", S_IRWXU);

	/* TODO use GNU getopt or similar to parse the output of
	 * this. Now this is quite a hack. */

	if (argc > 0) {
		if (strcmp("--binary",argv[1]) == 0 ||
		    strcmp("-b",argv[1]) == 0)
		{
			// Switch on binary mode and shift arguments by one
			binary = true;
			argc--;
			argv++;
		}
	}

	// TODO: figure out what should happen if an effect is not found by name
	if(argc < 3) {
		fprintf(stderr,"Missing effect and length arguments!\n\n"
			"Usage: %s [-b|--binary] name [length] [sensor_file] "
			"[custom_data]\n",prog);
	}
	else if(argc == 3) export_effect(find_effect(argv[1]), atof(argv[2]),
					 "", NULL, binary);
	else if(argc == 4) export_effect(find_effect(argv[1]), atof(argv[2]),
					 argv[3], NULL, binary);
	else if(argc == 5) export_effect(find_effect(argv[1]), atof(argv[2]),
					 argv[3], argv[4], binary);
}

void export_effect(const effect_t *effect, double length, const char *sensor_path, const char *data, bool binary) {
	const int size = 50;
	char filename[size];
	uint8_t use_sensors = strlen(sensor_path) > 0;

	json_t *distance1;
	json_t *distance2;
	json_t *ambient_light;
	json_t *sound_pressure_level;

	/* Attach custom data */
	custom_data = data;

	/* Parse sensor json */
	if(use_sensors) {
		json_error_t error;
		json_t *root = json_load_file(sensor_path, 0, &error);

		if(!root) {
			fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
		}
		else if(!json_is_object(root)) {
			fprintf(stderr, "error: root is not an object\n");
		}
		else {
			json_unpack(root, "{s:o, s:o, s:o, s:o}",
				"distance1", &distance1,
				"distance2", &distance2,
				"ambient_light", &ambient_light,
				"sound_pressure", &sound_pressure_level
			);
		}
	}

	/* Increment frame counter at the rate desired by the
	   effect. However, limit it to 25 fps to keep it compatible
	   with the serial port speed. 5 * 8 ms (25 fps) */
	const uint16_t drawing_time = effect->minimum_ticks < 5 ? 5 : effect->minimum_ticks;
	const uint8_t fps = 125/drawing_time;
	
	int bytes = snprintf(filename, size, "exports/%s.%s", effect->name, 
			     binary ? "elo" : "json");
	assert(bytes <= size);

	printf("Exporting %f seconds of %s to file %s\n",
		length,
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

	// TODO handle errors on file operations!

	if (binary) {
		// Hard coded frame size
		fputs("EV1",f);
		fputc(fps,f);
		fputc(GS_BUF_BYTES >> 8,f);
		fputc(GS_BUF_BYTES,f);
	} else {
		// Draw the frames
		fprintf(f,"{\"fps\":%d,\"geometry\":[%d,%d,%d],\"frames\":[[",
			fps,LEDS_X,LEDS_Y,LEDS_Z);
	}

	int i;
	for (i = 0, ticks = 0; i < (int)(fps*length); ticks += drawing_time, i++) {
		if(use_sensors) {
			sensors.distance1 = json_integer_value(json_array_get(distance1, i));
			sensors.distance2 = json_integer_value(json_array_get(distance2, i));
			sensors.ambient_light = json_integer_value(json_array_get(ambient_light, i));
			sensors.sound_pressure_level = json_integer_value(json_array_get(sound_pressure_level, i));
		}

		if(effect->draw != NULL) effect->draw();

		// Flip buffers to better simulate the environment
		gs_buf_swap();

		// Export stuff
		if (binary) {
			// Write the raw buffer
			fwrite(gs_buf_front,GS_BUF_BYTES,1,f);
		} else {
			for (int j=0; j<GS_BUF_BYTES; j+=3) {
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
	}

	// Return buffers back to original
	if (!effect->flip_buffers) gs_buf_front = old_front;

	if (!binary) {
		fseek(f,-2,SEEK_CUR); // TODO handle errors
		fputs("]}\n",f); // TODO handle errors
	}
	fclose(f); // TODO handle errors
}
