#define CRONTAB_SIZE 10

void get_crontab_entry(struct event *p, uint8_t i);
void truncate_crontab(uint8_t n);
void set_crontab_entry(struct event *p,uint8_t i);
