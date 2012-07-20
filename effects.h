/* 2D plotting function. Takes frame number, x coordinate, y
 * coordinate, and returns intensity value from 0 to
 * max_intensity. May have multiple definitions. */
typedef int(*plot_func_t)(int,int);

void effect_2d_plot(plot_func_t f);
int plot_sine(int x, int y);
int plot_constant(int x, int y);
void effect_layers_tester(void);
void clear_buffer(void);

extern uint16_t ticks;
