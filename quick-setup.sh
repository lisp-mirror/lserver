#!/bin/sh

LSERVER_HOME="${LSERVER_HOME:-$HOME/.lserver}"
command_dir="$LSERVER_HOME/commands"

mkdir -p "$LSERVER_HOME"

cat << EOF >> "$LSERVER_HOME/lserverrc.lisp"
(in-package #:lserver)

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
EOF

mkdir -p "$command_dir"

cat << EOF > "$command_dir/eval.lisp"
;;;; eval.lisp

(in-package #:lserver)

(file-command (lambda (args)
                (dolist (arg args t)
                  (eval (with-standard-io-syntax
                          (let ((*package* (find-package "LSERVER")))
                            (read-from-string arg))))))
              "Evaluate arguments as Lisp forms.")
EOF

cat << EOF > "$command_dir/say.lisp"
;;;; say.lisp

(in-package #:lserver)

(file-command (lambda (args)
                (loop for value in (multiple-value-list (eval (with-standard-io-syntax
                                                                (let ((*package* (find-package "LSERVER")))
                                                                  (read-from-string (first args))))))
                      do (format t "~A~%" value)
                      finally (finish-output) (return t)))
              "Evaluate the argument as a Lisp form and print the values on separate lines.")
EOF
