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

int main(int argc, char **argv) {
    //set_dispatch_functions(isatty(0) ? fgets_from_stdin : fread_from_stdin);
    init_readline();
    set_dispatch_functions();
    char socket_name[1024];
    {
        char *s = getenv("LSERVER_HOME");
        if (s) {
            if (strlen(s) > 1000) {
                fprintf(stderr, "Too long LSERVER_HOME");
                return EXIT_FAILURE;
            }
            strncpy(socket_name, s, 1024);
        } else {
            s = getenv("HOME");
            if (strlen(s) > 1000) {
                fprintf(stderr, "Too long homedir (?!)");
                return EXIT_FAILURE;
            }
            strncpy(socket_name, s, 1024);
            strcat(socket_name, "/.lserver");
        }
        s = getenv("LSERVER_SOCKET");
        if (s == NULL) s = "default";
        if (strlen(socket_name) + strlen("/tmp/") + strlen(s) > 1023) {
                fprintf(stderr, "Too long everything (?!)");
                return EXIT_FAILURE;
        }
        strcat(socket_name, "/tmp/");
        strcat(socket_name, s);
    }

    struct session s = {0};
    s.cont = 1;
    s.argv0 = argv[0];
    s.lisp_argc = argc - 1;
    s.lisp_argv = argv + 1;

    struct sockaddr_un servaddr;

    s.fd = socket(AF_LOCAL, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sun_family = AF_LOCAL;
    strcpy(servaddr.sun_path, socket_name);

    if (connect(s.fd, (struct sockaddr *) &servaddr, sizeof(servaddr))) {
        perror("connect");
        fprintf(stderr, "Cannot connect to server on socket %s\n", socket_name);
        return EXIT_FAILURE;
    }

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
