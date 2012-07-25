/**
 * JSON exporter for non-embedded use.
 */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/stat.h>
#include "../effect_utils.h"
#include "../effects.h"

uint8_t GSdata[768]={0x00};
uint8_t GSdata2[768]={0x00};
uint8_t *FrontBuffer = GSdata;
uint8_t *BackBuffer = GSdata2;

void export_effect(const effect_t *effect);

int main(int argc, char **argv) {
	mkdir("exports", S_IRWXU);

	for (int i=0; i<effects_len; i++) {
		export_effect(&effects[i]);
	}
}

void export_effect(const effect_t *effect) {
	const int size = 50;
	char filename[size];

	/* Increment frame counter always by 20 milliseconds
	   to simulate slow drawing. */
	const uint16_t drawing_time = 20;
	
	int bytes = snprintf(filename, size, "exports/%s.json", effect->name);
	assert(bytes <= size);

	printf("Exporting %d milliseconds of %s to file %s\n",effect->length, effect->name, filename);

	FILE *f = fopen(filename,"w");
	if (f == NULL) {
		fprintf(stderr,"Unable to write to effect.dump\n");
		return;
	}

	if (effect->init != NULL) effect->init();

	// Draw the frames
	fputs("{\"fps\":25,\"geometry\":[8,8,8],\"frames\":[[",f); // TODO handle errors
	for (ticks=0; ticks < effect->length; ticks += drawing_time) {
		effect->draw();

		// Flip buffers to better simulate the environment
		uint8_t *tmp = FrontBuffer;
		FrontBuffer = BackBuffer;
		BackBuffer = tmp;

		// Export stuff
		for (int j=0; j<768; j+=3) {
			uint16_t fst = FrontBuffer[j] << 4 | FrontBuffer[j+1] >> 4;
			uint16_t snd = ((FrontBuffer[j+1] & 0x0f) << 8) | FrontBuffer[j+2];

			fprintf(f,"%f,%f,",(float)fst/4095,(float)snd/4095);
		}
		// Unwind last comma
		fseek(f,-1,SEEK_CUR); // TODO handle errors
		fputs("],[",f); // TODO handle errors
	}
	fseek(f,-2,SEEK_CUR); // TODO handle errors
	fputs("]}\n",f); // TODO handle errors
	fclose(f); // TODO handle errors
}
