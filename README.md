# lserver

## Using a standalone server executable

### Build prerequisites

* [SBCL](http://sbcl.org)
* [quicklisp](https://www.quicklisp.org/beta/)

### Building and installing

1. Download the tarball and unpack it under `~/quicklisp/local-projects`.

        $ cd ~/quicklisp/local-projects; wget -O lserver.tar.gz https://notabug.org/quasus/lserver/archive/master.tar.gz && tar xvf lserver.tar.gz && cd lserver

2. As you are in the `lserver` directory, build the server and the client.

        $ make

3. Install the server and client binaries.  You can specify a location by means of prefix, e. g.

        $ make install prefix="$HOME/usr"

will install both under `~/usr/bin/`.

### Running

By default, the home directory of the server is `$HOME/.lserver`.  This can be
overridden by means of the `LSERVER_HOME` environment variable.

The `LSERVER_SOCKET` variable overrides the socket name, which is `default` by
default.

## <a name=portacle></a>Using from Portacle

Instead of building a standalone executable, you can install the portable
Common Lisp development environment [Portacle](https://portacle.github.io/) and
run the server from there.  This way you let Portacle take care of SBCL and
quicklisp and moreover, you get live introspection and hacking with Slime.

In this case you don’t need to build the server.

### Installation

1. Install [Portacle](https://portacle.github.io/).

2. `cd` into the Portacle directory.

3. Download the tarball and unpack it under `quicklisp/local-projects`:

        $ cd all/quicklisp/local-projects; wget -O lserver.tar.gz https://notabug.org/quasus/lserver/archive/master.tar.gz && tar xvf lserver.tar.gz && cd lserver

4. Build and install the client:

        $ cd client
        $ make
        $ make install prefix="$HOME/usr"

   Of course, you can use a different prefix or no prefix altogether if you
   install it under `/usr/local`.

### Running

To run the server, start Portacle, load `lserver` by writing

        CL-USER> (ql:quickload "lserver")

at the prompt.  When you do this for the first time, `quicklisp` will fetch
dependencies, so you will need an internet connection.  Then run the server by
calling

        CL-USER> (lserver:run-server :background t)

You can specify an alternative home directory and/or socket name like this:

        CL-USER> (lserver:run-server :background t :home "~/alternative-home" :socket "mysock")


### Using from a Lisp environment

The system definition is found in the `server` directory.  You build the client
and run the server as explained in the section [Using from Portacle](#portacle).

## Client

        $ lcli --list-commands
        $ lcli [command] [command-arguments]

* `command` is the name of the piece of software to run.
* `command-arguments` are CLI options for the command. 

## Configuring

No commands are defined out of the box, so the server must be configured in
order to be of any use.  You do that by editing the `lserverrc.lisp` file
residing in the server home directory.

You can run the `quick-setup.sh` script in order to obtain a simple file-based
command system described below together with the `eval` and `say` commands.
This doesn’t mean that this is the preferable way of managing commands, that
the `eval` and `say` commands must be generally available or that
`lserver::file-command` is an ‘official’ way of distributing software.

The `lserverrc.lisp` file is a regular Lisp file loaded when the server starts.
Single-line comments start with a semicolon and multi-line comments go between
`#|` and `|#`.

You can use the `add-command` function to directly define commands in the
configuration file.  Here is a small configuration:

    (in-package #:lserver)

    (add-command "eval" (lambda (args)
                          (dolist (arg args t)
                            (eval (with-standard-io-syntax
                                    (let ((*package* (find-package "LSERVER")))
                                      (read-from-string arg))))))
                 "Evaluate the arguments as Lisp expressions")

    (add-command "say" (lambda (args)
                         (loop for value in (multiple-value-list (eval (with-standard-io-syntax
                                                                         (let ((*package* (find-package "LSERVER")))
                                                                           (read-from-string (first args))))))
                               do (format t "~A~%" value)
                               finally (finish-output) (return t))))

The first line specifies the namespace containing `lserver`-specific tools. Put
it at the top of the configuration file unless you know what you are doing.
The `add-command` function accepts a command name, a `main`-type function
accepting a list of command-line options, and an optional description. The
commands defined in the snippet evaluate arbitrary Lisp expressions and
optionally print the results, e. g.

    $ lcli eval '(write-line "Hello, world!")'
    Hello, world!
    $ lcli say '(+ 1 2)'
    3

Of course, you don’t want that if `lserver` is to be used as a system-wide daemon.

If you want to define commands in individual files, you can use the following setup:

    (defun file-command (function &optional description)
      (add-command (pathname-name *load-truename*) function description))

    (defparameter *path* (list (merge-pathnames #p"commands/*.lisp" (lserver-homedir-pathname))))

    (defun commands-from-path ()
      (dolist (glob *path*)
        (dolist (pathname (directory glob))
          (with-standard-io-syntax
            (let ((*package* (find-package "LSERVER")))
              (load pathname))))))

    (commands-from-path)

Instead of defining the `eval` and `say` commands directly in the configuration
file, you can create the following files `eval.lisp` and `say.lisp` under the
`commands` subdirectory:

    ;;;; eval.lisp

    (in-package #:lserver)

    (file-command (lambda (args)
                    (dolist (arg args t)
                      (eval (with-standard-io-syntax
                              (let ((*package* (find-package "LSERVER")))
                                (read-from-string arg))))))
                  "Evaluate arguments as Lisp forms.")


    ;;;; say.lisp

    (in-package #:lserver)

    (file-command (lambda (args)
                    (loop for value in (multiple-value-list (eval (with-standard-io-syntax
                                                                    (let ((*package* (find-package "LSERVER")))
                                                                      (read-from-string (first args))))))
                          do (format t "~A~%" value)
                          finally (finish-output) (return t)))
                  "Evaluate the argument as a Lisp form and print the values on separate lines.")

Observe that `file-command` names commands after files, so you can easily rename commands by just renaming files.

If you add new command files while the server is running, you can run

    $ lcli eval '(commands-from-path)'

for a live update.

## Writing software

`lserver` commands can be defined on top of `lserver`-unaware functions.  When
the command is executed, the associated function is applied to the list of
provided command arguments with the following dynamic bindings:

* `*standard-input*`, `*standard-output*`, and `*standard-error*` are
  identified with client’s standard streams;
* `*query-io*` corresponds to stdin and stderr;
* `*default-pathname-defaults*` is client’s working directory,
* `*package*` is the `LSERVER` package.

The *exit code* is determined by the returned value: if the function returns an
integer, it is used as the exit code, if the function returns a non-integer
true value, the exit code is 0 and 1 otherwise.  If something goes wrong, the
client dies with a nonzero exit code, so such codes are not quite reliable.
