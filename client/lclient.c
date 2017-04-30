#include <errno.h>
#include <stdbool.h>
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

#define EOF_CODE 0
#define CHARACTER_CODE 1
#define LINE_CODE 2
#define LINE_PART_CODE 3
#define READ_ERROR_CODE 4
#define FLUSH_ERROR_CODE 5
#define WRITTEN_CODE 6
#define WRITE_ERROR_CODE 7
#define CMD_EXIT 0
#define CMD_READ_CHAR 1
#define CMD_READ_LINE 2
#define CMD_PRINT_STDOUT 3
#define CMD_PRINT_STDERR 4
#define MAX_DATA_SIZE 1024
#define HEADER_SIZE 3

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


/*
 * Communication: sending and receiving messages.
 * A message from the point of view of transfer: header & optional data
 * header: constant length
 * data: variable length <= MAX_DATA_SIZE
 * the header contains the command code (a small integer) and the length of the data
 * The header itself is interesting only in the context of sending/receiving
 * Implementation: struct message = information + allocated buffers.
 * code, data_len & data are `public' members
 * header is a `private' member
 * header is automatically calculated before sending.
 */

struct message {
    char header[HEADER_SIZE];
    char data[MAX_DATA_SIZE];
    char code;
    size_t data_len;
};

// < 0 on error, see errno.
int send_message(int fd, struct message *msg) {
    msg->header[0] = msg->code;
    msg->header[1] = (msg->data_len >> 8) & 0xFF;
    msg->header[2] = msg->data_len % 0xFF;
    if (writen(fd, msg->header, HEADER_SIZE) < 0) return -1;
    if (writen(fd, msg->data, msg->data_len) < 0) return -2;
    return 0;
}

struct bag {
    int fd;
    struct message in_msg;
    struct message out_msg;
    char mode;
    int exit_code;
    fd_set rset;
    char *argv0;
    int lisp_argc;
    char **lisp_argv;
    char cwd[1024];
};



int read_send_line(int fd, struct message *msg) {
    size_t len;
    bool proceed = true;
    while (proceed) {
        if (fgets(msg->data, MAX_DATA_SIZE, stdin)) {
            //fputs(msg->data);
            len = strlen(msg->data);
            if (msg->data[len - 1] == '\n') {
                msg->code = LINE_CODE;
                msg->data_len = len - 1;
                proceed = false;
            } else {
                msg->code = LINE_PART_CODE;
                msg->data_len = len;
            }
        } else if (ferror(stdin)) {
            msg->code = READ_ERROR_CODE;
            int2bytes(errno, msg->data);
            msg->data_len = sizeof(int);
            proceed = false;
        } else {
            msg->code = EOF_CODE;
            msg->data_len = 0;
            proceed = false;
        }
    if (send_message(fd, msg) < 0); return -1;
    }
    return 0
}

/*
> 0: ok, bytes read
0: eof
-1: error
*/
int read_utf8_sequence(int fd, char *buf, int *err) {
    int c = getchar();
    if (c == EOF) goto input_error;
    buf[0] = c;
    char more;
    if (buf[0] >> 7 == 0) more = 0;
    else if (buf[0] >> 5 == 6) more = 1;
    else if (buf[0] >> 4 == 14) more = 2;
    else more = 3;
    for (int i = 0; i < more; ++i) {
        c = getchar();
        if (c == EOF) goto input_error;
        buf[i] = c;
    }
    return more + 1;
input_error:
    *err = errno;
    return ferror(stdin) ? -1 : 0;
}

int read_send_utf8(int fd, struct message *msg) {
    int err;
    int n = read_utf8_sequence(fd, msg->data, &err);
    if (n > 0) {
        msg->code = CHARACTER_CODE;
        msg->data_len = n;
    } else if (n == 0) {
        msg->code = EOF_CODE;
        msg->data_len = 0;
    } else {
        msg->code = READ_ERROR_CODE;
        int2bytes(err, msg->data);
        msg->data_len = sizeof(int);
    }
    return send_message(fd, msg);
}

/*
 * Never mind errno, if we can't read an order there's nothing we can do on our
 * own; return a negative value & die asap.
 */
int read_order(int fd, struct message *msg) {
    int n = readn(fd, msg->header, HEADER_SIZE);
    if (n < HEADER_SIZE) return -1;
    msg->code = msg->header[0];
    msg->data_len = (unsigned char) msg->header[1] + (((unsigned char) msg->header[2]) << 8);
    if (msg->data_len > 0) {
        if (readn(fd, msg->data, msg->data_len) < msg->data_len) {
            return -2;
        }
    }
    return HEADER_SIZE + msg->data_len;
}

int print_to_fd(struct bag *b, int output_fd) {
    int err;
    if (b->in_msg.data_len > 0) {
        if (writen(output_fd, b->in_msg.data, b->in_msg.data_len) < 0) {
            err = errno;
            b->out_msg.code = WRITE_ERROR_CODE;
            int2bytes(err, b->out_msg.data);
            b->out_msg.data_len = sizeof(int);
            return send_message(b->fd, &(b->out_msg));
        }
    }
    if (fflush(NULL) == EOF) {
        err = errno;
        b->out_msg.code = FLUSH_ERROR_CODE;
        int2bytes(err, b->out_msg.data);
        b->out_msg.data_len = sizeof(int);
        return send_message(b->fd, &(b->out_msg));
    }
    b->out_msg.code = WRITTEN_CODE;
    b->out_msg.data_len = 0;
    return send_message(b->fd, &(b->out_msg));
}

int bytes2int(char *bytes) {
    int n = 0;
    for (int i = 0; i < sizeof(int); ++i) {
        n = (n << 8) + (unsigned char) bytes[i];
    }
    return n;
}

int dispatch_order(struct bag *b) {
    char code = b->in_msg.code;
    if (code == CMD_EXIT) {
        b->mode = 'q';
        b->exit_code = bytes2int(b->in_msg.data);
        return 0;
    }
    if (code == CMD_READ_LINE) return read_send_line(b->fd, &(b->out_msg));
    if (code == CMD_READ_CHAR) return read_send_utf8(b->fd, &(b->out_msg));
    if (code == CMD_PRINT_STDOUT) return print_to_fd(b, 1);
    if (code == CMD_PRINT_STDERR) return print_to_fd(b, 2);
    return -1;
}

void print_usage(void) {
    fprintf(stderr, "usage: lclient [-s, --sock SOCKET] [--] [LISP_ARGUMENTS]");
}

int main(int argc, char **argv) {
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
        
    struct bag b = {0};
    b.mode = 'o';
    b.argv0 = argv[0];
    b.lisp_argc = argc - lisp_argv_start;
    b.lisp_argv = argv + lisp_argv_start;
    if(!getcwd(b.cwd, 1024)) {
        perror("getcwd error");
        return EXIT_FAILURE;
    }

    struct sockaddr_un servaddr;

    b.fd = socket(AF_LOCAL, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sun_family = AF_LOCAL;
    strcpy(servaddr.sun_path, socket_name);

    connect(b.fd, (struct sockaddr *) &servaddr, sizeof(servaddr));

    while (b.mode != 'q') {
        if (read_order(b.fd, &(b.in_msg)) < 0) return EXIT_FAILURE;
        if (dispatch_order(&b) < 0) return EXIT_FAILURE;
    }

    return b.exit_code;
}
