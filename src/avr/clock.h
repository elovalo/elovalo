uint16_t centisecs(void);
void reset_time(void);

/* Time functions are modelled after POSIX */
typedef uint32_t time_t;

time_t time(time_t *t);
int stime(time_t *t);
time_t unsafe_time(time_t *t);
