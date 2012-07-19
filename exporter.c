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
extern int frame_counter;

int main(int argc, char **argv) {

	FILE *f = fopen("effect.dump","wb");
	if (f == NULL) {
		printf("Unable to write to effect.dump\n");
		return 1;
	}

	// Draw the frames
	for (int i=0; i<FRAME_COUNT; i++) {
		// Call the drawing function
		effect_2d_plot(&plot_sine);

		// Flip buffers to better simulate the environment
		uint8_t *tmp = FrontBuffer;
		FrontBuffer = BackBuffer;
		BackBuffer = tmp;

		// Frame counter
		frame_counter++;

		// Export stuff
		fwrite(FrontBuffer,768,1,f); // TODO handle errors
	}

	fclose(f); // TODO handle errors
	return 0;
}
