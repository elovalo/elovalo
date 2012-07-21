/* 2D plotting function. Takes frame number, x coordinate, y
 * coordinate, and returns intensity value from 0 to
 * max_intensity. May have multiple definitions. */
typedef uint16_t(*plot_func_t)(uint8_t,uint8_t);

void effect_2d_plot(plot_func_t f);
void effect_layers_tester(void);

uint16_t plot_sine(uint8_t x, uint8_t y);
uint16_t plot_constant(uint8_t x, uint8_t y);

void clear_buffer(void);

extern uint16_t ticks;
