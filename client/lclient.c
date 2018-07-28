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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>
#include "interpreter.h"

void print_usage(void) {
    fprintf(stderr, "usage: lclient [-s, --sock SOCKET] [--] [LISP_ARGUMENTS]");
}

int main(int argc, char **argv) {
    //set_dispatch_functions(isatty(0) ? fgets_from_stdin : fread_from_stdin);
    init_readline();
    set_dispatch_functions();
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
