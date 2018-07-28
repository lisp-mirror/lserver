# lserver

## Running the server

### Using a standalone executable

#### Build prerequisites

* SBCL
* [quicklisp](https://www.quicklisp.org/beta/)

#### Building

1. Download the tarball and unpack it under `~/quicklisp/local-projects`.

        $ cd ~/quicklisp/local-projects; wget https://notabug.org/quasus/lserver/archive/master.tar.gz && tar xvf master.tar.gz

2. In the `server` directory, run `make`.

        $ cd lserver/server; make

3. Copy the `lserver` executable elsewhere.

#### Running

By default, the home directory of the server is `$HOME/.lserver`.  This can be
overridden by means of the `LSERVER_HOME` environment variable.

When starting, `lserver` loads the configuration file `lserverrc.lisp`.  You
should put something there in order for the server to be of any use (see
[Configuring](#configuring)).

### Using from Portacle

Instead of building a standalone executable, you can install the portable
Common Lisp development environment [Portacle](https://portacle.github.io/) and
run the server from there.  This way you let Portacle take care of SBCL and
quicklisp and moreover, you get live introspection and hacking with Slime.

1. Install [Portacle](https://portacle.github.io/).

2. `cd` into the Portacle directory.

3. Download the tarball and unpack it under `quicklisp/local-projects`. 

    $ cd all/quicklisp/local-projects; wget https://notabug.org/quasus/lserver/archive/master.tar.gz && tar xvf master.tar.gz

### Using from a Lisp environment

The system definition is found in the `server` directory.

## Installing the client

### Prerequisites

* `gcc`
* `readline`

### Building
1. `cd` to `lserver/client`.
2. Run `make`.
3. Copy the `lcli` executable elsewhere.

### Usage

    $ lcli --list-commands
    $ lcli [command] [command-arguments]

* `command` is the name of the piece of software to run.
* `command-arguments` are CLI options for the command. 

## <a name=configuring>Configuring

Individual pieces of Lisp software are represented by the `lserver`-specific
concept of *commands*.  `lserver` knows no commands out of the box and must be
configured using the configuration file.

The configuration file `lserverrc.lisp` residing in the `lserver` home
directory is a regular Lisp file.  Single-line comments start with a semicolon
and multi-line comments go between `#|` and `|#`.

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

If you want to define commands in individual files, you can use the following code:

    (defun file-command (function &optional description)
      (add-command (pathname-name *load-truename*) function description))

    (defvar *path* (list (merge-pathnames #p"commands/" (lserver-homedir-pathname))))

    (defun commands-from-path ()
      (dolist (directory *path*)
        (dolist (file (uiop:directory-files directory "*.lisp"))
          (let ((*package* (find-package "LSERVER")))
            (load file)))))

    (commands-from-path)

Then instead of defining the `eval` and `say` commands in the configuration
file itself, you can create the following files `eval.lisp` and `say.lisp`
under the `commands` subdirectory:


    ;;;; eval.lisp

    (in-package #:lserver)

    (file-command (lambda (args)
                    (dolist (arg args t)
                      (eval (with-standard-io-syntax
                              (let ((*package* (find-package "LSERVER")))
                                (read-from-string arg))))))
                  "Eval arguments as Lisp forms.")


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

If you add new command files, you can run

    $ lcli eval '(commands-from-path)'

to update the commands.

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
