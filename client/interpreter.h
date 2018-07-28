#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <stdbool.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/select.h>

#define MAX_DATA_SIZE 4096

#define PONG 0

void init_readline();
void set_dispatch_functions();

struct session {
    bool cont;
    unsigned char cmd;
    char data[MAX_DATA_SIZE];
    uint32_t i;
    int fd;
    fd_set rset;
    char* string;
    char *argv0;
    int lisp_argc;
    char **lisp_argv;
    char *line_read;
    size_t line_read_len;
    size_t line_read_start;
    char readline_prompt[256];
    size_t readline_prompt_len;
    bool readline_prompt_fail;
};

typedef int (*dispatch_fun_t)(struct session *);

#define NUM_COMMANDS 18

extern dispatch_fun_t dispatch_functions[NUM_COMMANDS];

int cmd_from_lisp(struct session *s);

#endif
