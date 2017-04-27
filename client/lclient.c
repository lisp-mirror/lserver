#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>

#include <stdio.h>

#define EOF_CODE 0
#define CHARACTER_CODE 1
#define LINE_CODE 2
#define LINE_PART_CODE 3
#define READ_ERROR_CODE 4
#define CMD_EXIT 0
#define CMD_READ_CHAR 1
#define CMD_READ_LINE 2
#define CMD_PRINT_STDOUT 3
#define CMD_PRINT_STDERR 4
#define MAX_DATA_SIZE 1024
#define HEADER_SIZE 3

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

int send_message(int fd, struct message *msg) {
    msg->header[0] = msg->code;
    msg->header[1] = (msg->data_len >> 8) & 0xFF;
    msg->header[2] = msg->data_len % 0xFF;
    if (writen(fd, msg->header, HEADER_SIZE) == -1) return -1;
    if (writen(fd, msg->data, msg->data_len) == -1) return -2;
    return 0;
}

void read_send_line(int fd, struct message *msg) {
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
        } else if (feof(stdin)) {
            msg->code = EOF_CODE;
            msg->data_len = 0;
            proceed = false;
        } else {
            msg->code = EOF_CODE;
            msg->data_len = 0;
            proceed = false;
        }
        send_message(fd, msg);
    }
}

// retur number of bytes
// 0: eof
// -1: error
int read_utf8_sequence(int fd, char *buf) {
    // TODO use readn
    int n = read(0, buf, 1);
    if (n < 0) return - 1;
    if (n == 0) return 0;
    char first = buf[0];
    char more;
    if (first >> 7 == 0) {
        more = 0;
    } else if (first >> 5 == 6) {
        more = 1;
    } else if (first >> 4 == 14) {
        more = 2;
    } else {
        more = 3;
    }
    if (more > 0) {
        n = read(0, buf + 1, more);
        if (n < 0) return -1;
        if (n < more) return 0;
        return 1 + more;
    }
    return 1;
}

void read_send_utf8(int fd, struct message *msg) {
    char n = read_utf8_sequence(fd, msg->data);
    if (n > 0) {
        msg->code = CHARACTER_CODE;
        msg->data_len = n;
    } else if (n == 0) {
        msg->code = EOF_CODE;
        msg->data_len = 0;
    } else {
        // TODO: send errno in the data
        msg->code = READ_ERROR_CODE;
        msg->data_len = 0;
    }
    send_message(fd, msg);
}

int read_order(int fd, struct message *msg) {
    int n = readn(fd, msg->header, HEADER_SIZE);
    if (n < HEADER_SIZE) return -1;
    msg->code = msg->header[0];
    msg->data_len = (msg->header[1] << 8) + msg->header[2];
    if (msg->data_len > 0) {
        if (readn(fd, msg->data, msg->data_len) < msg->data_len) {
            return -1;
        }
    }
    return HEADER_SIZE + msg->data_len;
}

void print_stdout(int fd, struct message *msg) {
    writen(1, msg->data, msg->data_len);
    // in the future we won't flush immediately
    fflush(NULL);
}

void print_stderr(int fd, struct message *msg) {
    writen(2, msg->data, msg->data_len);
    // in the future we won't flush immediately
    fflush(NULL);
}

void dispatch_order(int fd, struct message *in_msg, struct message *out_msg) {
    if (in_msg->code == CMD_EXIT) exit(0);
    else if (in_msg->code == CMD_READ_LINE) read_send_line(fd, out_msg);
    else if (in_msg->code == CMD_READ_CHAR) read_send_utf8(fd, out_msg);
    else if (in_msg->code == CMD_PRINT_STDOUT) print_stdout(fd, in_msg);
    else if (in_msg->code == CMD_PRINT_STDERR) print_stderr(fd, in_msg);
    else exit(1);
}

int main(int argc, char **argv) {
    if (argc == 1) {
        fprintf(stderr, "Socket needed.\n");
        return EXIT_FAILURE;
    }
    int sockfd;
    struct sockaddr_un servaddr;
    struct message in_msg, out_msg;

    sockfd = socket(AF_LOCAL, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sun_family = AF_LOCAL;
    strcpy(servaddr.sun_path, argv[1]);

    connect(sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr));

    /*
    set_message(&msg, LINE_PART_CODE, "hello ", 6);
    send_message(sockfd, &msg);

    */
    //set_message(&msg, LINE_CODE, "world", 5);
    //send_message(sockfd, &msg);

    //read_send_line(sockfd);
    //read_send_utf8(sockfd);
    

    // not working?
    int n;
    while (n = read_order(sockfd, &in_msg) >= 0) {
        //printf("%d\n", n);
        dispatch_order(sockfd, &in_msg, &out_msg);
    }
    // communication error
    return -1;
}
