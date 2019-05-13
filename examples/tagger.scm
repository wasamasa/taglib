(import scheme)
(import (chicken base))
(import (chicken condition))
(import (chicken format))
(import (chicken process-context))
(import (chicken string))
(import getopt-long)
(import (prefix taglib taglib:))

(define (die #!rest messages)
  (for-each
   (lambda (message)
     (display message (current-error-port))
     (newline (current-error-port)))
   messages)
  (exit 1))

(define (string-split-first string separator)
  (let ((index (substring-index separator string)))
    (if index
        (let ((s1 (substring string 0 index))
              (s2 (substring string (+ index (string-length separator)))))
          (if (zero? (string-length s2))
              (list s1)
              (list s1 s2)))
        (list string))))

(define options
  `((title
     "Set title"
     (value #t)
     (single-char #\t))
    (artist
     "Set artist"
     (value #t)
     (single-char #\a))
    (album
     "Set album"
     (value #t)
     (single-char #\A))
    (comment
     "Set comment"
     (value #t)
     (single-char #\c))
    (genre
     "Set genre"
     (value #t)
     (single-char #\g))
    (year
     "Set year"
     (value (required "NUMBER")
            (transformer ,string->number))
     (single-char #\y))
    (track
     "Set track number"
     (value (required "NUMBER")
            (transformer ,string->number))
     (single-char #\n))
    (raw
     "Set raw key:value pair"
     (value (required "KEY:VALUE")
            (transformer ,(lambda (s) (string-split-first s ":"))))
     (single-char #\r))
    (help
     "Prints this help"
     (single-char #\h))))

(define usage-hint
  (format "Usage: ~a [options] <files>" (program-name)))

(when (null? (command-line-arguments))
  (die usage-hint))
(let* ((opts
        (condition-case
         (getopt-long (command-line-arguments) options)
         (e (exn)
            (die (format "Error: ~a: ~a"
                         ((condition-property-accessor 'exn 'message) e)
                         ((condition-property-accessor 'exn 'arguments) e))
                 usage-hint
                 (usage options)))))
       (help? (alist-ref 'help opts))
       (paths (alist-ref '@ opts)))
  (when help?
    (print usage-hint)
    (display (usage options))
    (exit 0))
  (when (null? paths)
    (die "No input files specified" usage-hint))
  (for-each
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (when (taglib:file-valid? file)
         (print "Processing: " path "...")
         (for-each
          (lambda (opt)
            (let ((key (car opt))
                  (value (cdr opt)))
              (case key
                ((title) (set! (taglib:tag-property file 'title) value))
                ((artist) (set! (taglib:tag-property file 'artist) value))
                ((album) (set! (taglib:tag-property file 'album) value))
                ((comment) (set! (taglib:tag-property file 'comment) value))
                ((genre) (set! (taglib:tag-property file 'genre) value))
                ((year) (set! (taglib:tag-property file 'year) value))
                ((track) (set! (taglib:tag-property file 'track) value))
                ((raw)
                 (when (null? (cdr value))
                   (error "Raw value is unset"))
                 (let ((raw-key (car value))
                       (raw-value (cdr value)))
                   (set! (taglib:raw-tag-property file raw-key) raw-value))))))
          opts)
         (taglib:file-save! file))))
   paths))
