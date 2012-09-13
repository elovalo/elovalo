#define CRONTAB_SIZE 10

#include "cron.h"

void get_crontab_entry(struct event *p, uint8_t i);
void truncate_crontab(uint8_t n);
void set_crontab_entry(struct event *p,uint8_t i);
