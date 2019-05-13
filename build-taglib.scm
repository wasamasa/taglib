(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken format))
(import (chicken io))
(import (chicken process))
(import (chicken process-context))
(import (chicken string))

(define (library-flags var command)
  (or (get-environment-variable var)
      (let ((exit (system (string-append command " > /dev/null"))))
        (if (zero? exit)
            (let ((output (with-input-from-pipe command (cut read-string #f))))
              (when (eof-object? output)
                (error (format "Command didn't produce any output: ~a" command)))
              output)
            (error (format "Command failed with exit code ~s, set $~a" exit var))))))

(define csc (get-environment-variable "CHICKEN_CSC"))
(define taglib-cflags (library-flags "TAGLIB_CFLAGS" "(pkg-config --cflags taglib || echo '-I/usr/include/taglib')"))
(define taglib-ldlibs (library-flags "TAGLIB_LDLIBS" "(pkg-config --libs taglib || echo '-ltag')"))

(define args (list csc taglib-cflags taglib-ldlibs))
(define cmdline
  (string-append (apply format "~a -C ~a -L ~a " (map qs args))
                 (string-intersperse (map qs (command-line-arguments)) " ")))

(system cmdline)
