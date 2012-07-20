/**
 * JSON exporter for non-embedded use.
 */

#include <stdio.h>
#include <stdint.h>
#include "effects.h"

uint8_t GSdata[768]={0x00};
uint8_t GSdata2[768]={0x00};
uint8_t *FrontBuffer = GSdata;
uint8_t *BackBuffer = GSdata2;

#define FRAME_COUNT 100

int main(int argc, char **argv) {

	FILE *f = fopen("effect.dump","w");
	if (f == NULL) {
		fprintf(stderr,"Unable to write to effect.dump\n");
		return 1;
	}

	// Draw the frames
	fputs("{\"fps\":25,\"geometry\":[8,8,8],\"frames\":[[",f); // TODO handle errors
	for (int i=0; i<FRAME_COUNT; i++) {
		// Call the drawing function
		effect_2d_plot(&plot_sine);

		// Flip buffers to better simulate the environment
		uint8_t *tmp = FrontBuffer;
		FrontBuffer = BackBuffer;
		BackBuffer = tmp;

		/* Increment frame counter always by 20 milliseconds
		   to simulate slow drawing. */
		ticks+=20;

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
	return 0;
}
