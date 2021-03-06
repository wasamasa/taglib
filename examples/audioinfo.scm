(import scheme)
(import (chicken base))
(import (chicken pretty-print))
(import (chicken process-context))
(import (chicken string))
(import (prefix taglib taglib:))

(define (with-unit value unit)
  (if value
      (string-append (->string value) unit)
      "unknown"))

(define (inexact arg)
  (and (number? arg) (exact->inexact arg)))

(for-each
 (lambda (path)
   (let ((file (taglib:file-open path)))
     (when (and file (taglib:file-valid? file))
       (print "Processing: " path "...")
       (print "Length: " (with-unit (inexact (taglib:audio-property file 'length)) "s"))
       (print "Bitrate: " (with-unit (taglib:audio-property file 'bitrate) "kb/s"))
       (print "Samplerate: " (with-unit (taglib:audio-property file 'samplerate) "Hz"))
       (print "Channels: " (or (taglib:audio-property file 'channels) "unknown"))
       (print "Title: " (or (taglib:tag-property file 'title) "unknown"))
       (print "Artist: " (or (taglib:tag-property file 'artist) "unknown"))
       (print "Album: " (or (taglib:tag-property file 'album) "unknown"))
       (print "Comment: " (or (taglib:tag-property file 'comment) "unknown"))
       (print "Genre: " (or (taglib:tag-property file 'genre) "unknown"))
       (print "Year: " (or (taglib:tag-property file 'year) "unknown"))
       (print "Track: " (or (taglib:tag-property file 'track) "unknown"))
       (for-each
        (lambda (property)
          (let ((key (car property))
                (values (cdr property)))
            (print key ": " (string-intersperse values ","))))
        (taglib:raw-tag-properties file)))))
 (command-line-arguments))
