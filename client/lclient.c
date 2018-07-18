/*
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

#define _POSIX_C_SOURCE 1

#include <arpa/inet.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>

#include <stdio.h>

#include <readline/readline.h>
#include <readline/history.h>

#define MAX_DATA_SIZE 4096
#define PONG 0

// = 0: OK
// > 0: EOF
// < 0: error (errno)
// function readn
// * Copyright (c) 1996 W. Richard Stevens.  All rights reserved.
ssize_t                         /* Read "n" bytes from a descriptor. */
readn(int fd, void *vptr, size_t n)
{
    size_t  nleft;
    ssize_t nread;
    char   *ptr;

    ptr = vptr;
    nleft = n;
    while (nleft > 0) {
        if ( (nread = read(fd, ptr, nleft)) < 0) {
            if (errno == EINTR)
                nread = 0;      /* and call read() again */
            else
                return (-1);
        } else if (nread == 0)
            break;              /* EOF */

        nleft -= nread;
        ptr += nread;
    }
    return (n - nleft);         /* return >= 0 */
}

/* >=0: OK
 * <0 : error, errno.
 */
/* function writen
 * Copyright (c) 1996 W. Richard Stevens.  All rights reserved.
 */
/* Write "n" bytes to a descriptor. */
ssize_t writen(int fd, const void *vptr, size_t n) {
    size_t nleft;
    ssize_t nwritten;
    const char *ptr;

    ptr = vptr;
    nleft = n;
    while (nleft > 0) {
        if ( (nwritten = write(fd, ptr, nleft)) <= 0) {
            if (nwritten < 0 && errno == EINTR)
                nwritten = 0;   /* and call write() again */
            else
                return (-1);    /* error */
         }

         nleft -= nwritten;
         ptr += nwritten;
    }
    return (n);
}

char *int2bytes(int n, char *bytes) {
    for (int i = 0; i < sizeof(int); ++i) {
        bytes[i] = n & 0xFF;
        n >>= 8;
    }
    return bytes;
}

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

int cmd_from_lisp(struct session *s) {
    if (readn(s->fd, &(s->cmd), sizeof(char)) < 0) {
        perror("cmd_from_lisp");
        return EXIT_FAILURE;
    }
    return 0;
}

int dump_to_stream(struct session *s, FILE *stream, char *perror_string) {
    size_t n = s->i;
    if (isatty(STDIN_FILENO) && isatty(fileno(stream))) {
        char *pos = s->data + n - 1;
        size_t len = 0;
        for (; pos >= s->data; --pos, ++len) {
            if (*pos == '\n') {
                ++pos;
                s->readline_prompt_fail = false;
                break;
            }
        }
        if (pos < s->data) pos = s->data;
        if (s->readline_prompt_len + len < 256) {
            memcpy(s->readline_prompt + s->readline_prompt_len, pos, len);
            s->readline_prompt_len += len;
            *(s->readline_prompt + s->readline_prompt_len) = 0;
        } else {
            // pray such a long string is not going to be used as prompt
            *(s->readline_prompt) = 0;
            s->readline_prompt_len = 0;
            s->readline_prompt_fail = true;
        }
    }
    if (fwrite(s->data, 1, n, stream) != n) {
        perror(perror_string);
        return EXIT_FAILURE;
    }
    return 0;
}

int dump_to_stdout(struct session *s) {
    return dump_to_stream(s, stdout, "dump_to_stdout");
}

int dump_to_stderr(struct session *s) {
    return dump_to_stream(s, stderr, "dump_to_stderr");
}

int flush_stream(FILE *stream, char *perror_string) {
    if (fflush(stream)) {
        perror(perror_string);
        return EXIT_FAILURE;
    }
    return 0;
}

int flush_stdout(struct session *s) {
    return flush_stream(stdout, "flush_stdout");
}

int flush_stderr(struct session *s) {
    return flush_stream(stderr, "flush_stderr");
}

int fread_from_stdin(struct session *s) {
    s->i = (uint32_t) fread(s->data, 1, MAX_DATA_SIZE, stdin);
    if (s->i < MAX_DATA_SIZE && ferror(stdin)) {
        perror("fread_from_stdin");
        return EXIT_FAILURE;
    }
    return 0;
}

int fgets_from_stdin(struct session *s) {
    if (fgets(s->data, MAX_DATA_SIZE, stdin)) {
        s->i = (uint32_t) strlen(s->data);
    } else if(ferror(stdin)) {
        perror("fgets_from_stdin");
        return EXIT_FAILURE;
    } else {
        s->i = 0;
    }
    return 0;
}

int readline_from_stdin(struct session *s) {
    if (s->line_read_start >= s->line_read_len) {
        free(s->line_read);
        if (s->readline_prompt_fail) {
            s->line_read = readline("");
        } else {
            // kill the line, hopefully it's in the prefix
            printf("\33[2K\r");
            s->line_read = readline(s->readline_prompt);
        }
        // readline ends with a newline, so reset the prompt
        *(s->readline_prompt) = 0;
        s->readline_prompt_len = 0;
        s->readline_prompt_fail = false;
        if (s->line_read && *(s->line_read)) add_history(s->line_read);
        s->line_read_start = 0;
        s->line_read_len = (s->line_read) ? strlen(s->line_read) : 0;
        if (s->line_read) {
            *(s->line_read + s->line_read_len) = '\n';
            ++(s->line_read_len);
        }
    }
    if (s->line_read_start >= s->line_read_len) {
        s->i = 0;
        return 0;
    }
    if (s->line_read_len - s->line_read_start <= MAX_DATA_SIZE) {
        memcpy(s->data, s->line_read + s->line_read_start, s->line_read_len - s->line_read_start);
        s->i = s->line_read_len - s->line_read_start;
        s->line_read_start = s->line_read_len;
        return 0;
    }
    memcpy(s->data, s->line_read + s->line_read_start, MAX_DATA_SIZE);
    s->i = MAX_DATA_SIZE;
    s->line_read_start += MAX_DATA_SIZE;
    return 0;
}

// should perror be called by the caller instead?
int int_to_lisp(struct session *s) {
    uint32_t nw = htonl(s->i);
    if (writen(s->fd, &nw, sizeof(uint32_t)) < 0) {
        perror("int_to_lisp");
        return EXIT_FAILURE;
    }
    return 0;
}

int data_to_lisp (struct session *s) {
    if (int_to_lisp(s)) return EXIT_FAILURE; // send_int may call perror
    if (s->i > 0) {
        if (writen(s->fd, s->data, (size_t) s->i) < 0) {
            perror("data_to_lisp");
            return EXIT_FAILURE;
        }
    }
    return 0;
}

int string_to_lisp (struct session *s) {
    size_t len = strlen(s->string);
    s->i = (uint32_t) len;
    if (int_to_lisp(s)) return EXIT_FAILURE; // send_int may call perror
    if (len > 0) {
        if (writen(s->fd, s->string, len) < 0) {
            perror("string_to_lisp");
            return EXIT_FAILURE;
        }
    }
    return 0;
}

int save_lisp_argc (struct session *s) {
    s->i = (uint32_t) s->lisp_argc;
    return 0;
}

int save_lisp_arg (struct session *s) {
    if (s->i >= s->lisp_argc) {
        perror("save_lisp_arg");
        return EXIT_FAILURE;
    }
    s->string = s->lisp_argv[s->i];
    return 0;
}

int save_arg0 (struct session *s) {
    s->string = s->argv0;
    return 0;
}

int save_env (struct session *s) {
    s->string = getenv(s->data);
    s->i = (s->string != NULL);
    return 0;
}

int save_cwd (struct session *s) {
    if(!getcwd(s->data, MAX_DATA_SIZE)) {
        perror("getcwd error");
        return EXIT_FAILURE;
    }
    s->string = s->data;
    return 0;
}

int save_isatty (struct session *s) {
    s->i = isatty(0);
    return 0;
}

int quit(struct session *s) {
    s->cont = false;
    return 0;
}


int pong(struct session *s) {
    s->i = PONG;
    return int_to_lisp(s);
}

int int_from_lisp(struct session *s) {
    uint32_t n = 0;
    if (readn(s->fd, &n, sizeof(uint32_t)) < 0) {
        perror("int_from_lisp");
        return EXIT_FAILURE;
    }
    s->i = ntohl(n);
    return 0;
}

int data_from_lisp(struct session *s) {
    if (int_from_lisp(s)) {
        perror("data_from_lisp");
        return EXIT_FAILURE;
    }
    if (s->i >= MAX_DATA_SIZE) return EXIT_FAILURE;
    if (readn(s->fd, s->data, (size_t) s->i) < 0) {
        perror("data_from_lisp");
        return EXIT_FAILURE;
    }
    return 0;
}

typedef int (*dispatch_fun_t)(struct session *);

#define NUM_COMMANDS 18

dispatch_fun_t dispatch_functions[NUM_COMMANDS];

void set_dispatch_functions(dispatch_fun_t read_fn) {
    dispatch_functions[0] = quit;
    dispatch_functions[1] = pong;
    dispatch_functions[2] = int_from_lisp;
    dispatch_functions[3] = data_from_lisp;
    dispatch_functions[4] = int_to_lisp;
    dispatch_functions[5] = data_to_lisp;
    dispatch_functions[6] = read_fn;
    dispatch_functions[7] = dump_to_stdout;
    dispatch_functions[8] = flush_stdout;
    dispatch_functions[9] = dump_to_stderr;
    dispatch_functions[10] = flush_stderr;
    dispatch_functions[11] = string_to_lisp;
    dispatch_functions[12] = save_arg0;
    dispatch_functions[13] = save_lisp_argc;
    dispatch_functions[14] = save_lisp_arg;
    dispatch_functions[15] = save_env;
    dispatch_functions[16] = save_cwd;
    dispatch_functions[17] = save_isatty;
}

void print_usage(void) {
    fprintf(stderr, "usage: lclient [-s, --sock SOCKET] [--] [LISP_ARGUMENTS]");
}

void init_readline() {
    rl_bind_key ('\t', rl_insert);
}

int main(int argc, char **argv) {
    //set_dispatch_functions(isatty(0) ? fgets_from_stdin : fread_from_stdin);
    init_readline();
    set_dispatch_functions(isatty(0) ? readline_from_stdin : fread_from_stdin);
    int i = 1;
    int lisp_argv_start = argc;
    char *socket_name = NULL;
    while (i < argc) {
        if (strcmp(argv[i], "--") == 0) {
            lisp_argv_start = i + 1;
            break;
        } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage();
            return EXIT_SUCCESS;
        } else if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--socket") == 0) {
            if (i + 1 == argc || strcmp(argv[i+1], "--") == 0) {
                fprintf(stderr, "Socket name missing after %s.", argv[i]);
                print_usage();
                return EXIT_FAILURE;
            }
            socket_name = argv[i + 1];
            i += 2;
        } else {
            fprintf(stderr, "Unknown option: %s.", argv[i]);
            print_usage();
            return EXIT_FAILURE;
        }
    }
    if (!socket_name) socket_name = getenv("LSERVER_SOCKET");
    if (!socket_name) {
        fprintf(stderr, "Socket needed.\n");
        return EXIT_FAILURE;
    }
        
    struct session s = {0};
    s.cont = 1;
    s.argv0 = argv[0];
    s.lisp_argc = argc - lisp_argv_start;
    s.lisp_argv = argv + lisp_argv_start;

    struct sockaddr_un servaddr;

    s.fd = socket(AF_LOCAL, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sun_family = AF_LOCAL;
    strcpy(servaddr.sun_path, socket_name);

    connect(s.fd, (struct sockaddr *) &servaddr, sizeof(servaddr));

    while (s.cont) {
        if (cmd_from_lisp(&s)) return EXIT_FAILURE;
        if (s.cmd >= NUM_COMMANDS || s.cmd < 0) {
            fprintf(stderr, "Unknown command code %d.\n", (int) s.cmd);
            return EXIT_FAILURE;
        }
        //printf("cmd %d\n", s.cmd);
        if ((*dispatch_functions[s.cmd])(&s)) return EXIT_FAILURE;
    }

    return (int) s.i;
}
