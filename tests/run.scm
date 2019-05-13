(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken pathname))
(import (chicken string))
(import (prefix taglib taglib:))
(import (srfi 1))
(import test)

(define test-files '("null.flac" "null.mp3" "null.ogg"))
(define audio-properties '(length bitrate samplerate channels))
(define tag-properties '(title artist album comment genre year track))
(define tag-values '("Tests" "Tester" "Testing" "Tested" "Other" 1970 1 "Tester"))
(define raw-tag-properties '("TITLE" "ARTIST" "ALBUM" "COMMENT" "GENRE" "DATE" "TRACKNUMBER" "ALBUMARTIST"))
(define raw-tag-values '(("Tests") ("Tester") ("Testing") ("Tested") ("Other") ("1970") ("1") ("Tester")))

(define (call-with-test-file src thunk)
  (let ((dest (pathname-replace-file src "tmp")))
    (copy-file src dest #t)
    (thunk dest)
    (delete-file* dest)))

(define (call-with-test-files thunk)
  (for-each
   (lambda (src)
     (call-with-test-file src thunk))
   test-files))

(test-group "Sanity check"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (test-assert file)
       (test-assert (taglib:file-valid? file))
       (taglib:file-free! file)
       (test #f (taglib:file-valid? file))))))

(test-group "Getting audio properties"
  (call-with-test-files
   (lambda (path)
     (let* ((file (taglib:file-open path))
            (props (taglib:audio-properties file)))
       (for-each
        (lambda (prop)
          (test-assert (taglib:audio-property file prop))
          (test-assert (and (assoc prop props) (alist-ref prop props))))
        audio-properties)
       (test-error (taglib:audio-property file 'bogus))
       (test #f (assoc 'bogus props))))))

(test-group "Getting empty tag properties"
  (call-with-test-files
   (lambda (path)
     (let* ((file (taglib:file-open path))
            (props (taglib:tag-properties file)))
       (for-each
        (lambda (prop)
          (test #f (taglib:tag-property file prop))
          (test-assert (and (assoc prop props) (not (alist-ref prop props)))))
        tag-properties)))))

(test-group "Writing and discarding tag properties"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (set! (taglib:tag-property file prop) value)
          (test value (taglib:tag-property file prop)))
        tag-properties
        tag-values))
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop)
          (test #f (taglib:tag-property file prop)))
        tag-properties)))))

(test-group "Writing and saving tag properties"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (set! (taglib:tag-property file prop) value)
          (test value (taglib:tag-property file prop)))
        tag-properties
        tag-values)
       (taglib:file-save! file))
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (test value (taglib:tag-property file prop)))
        tag-properties
        tag-values)))))

(test-group "Writing and clearing tag properties"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (set! (taglib:tag-property file prop) value))
        tag-properties
        tag-values)
       (taglib:file-save! file))
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (test value (taglib:tag-property file prop))
          (set! (taglib:tag-property file prop) #f)
          (test #f (taglib:tag-property file prop)))
        tag-properties
        tag-values)))))

(test-group "Getting empty raw tag properties"
  (call-with-test-files
   (lambda (path)
     (let* ((file (taglib:file-open path))
            (props (taglib:raw-tag-properties file)))
       (for-each
        (lambda (prop)
          (test '() (taglib:raw-tag-property file prop))
          (test #f (alist-ref prop props))
          (test #f (taglib:raw-tag-property-exists? file prop)))
        raw-tag-properties)))))

(test-group "Writing and discarding raw tag properties"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (set! (taglib:raw-tag-property file prop) value)
          (test value (taglib:raw-tag-property file prop)))
        raw-tag-properties
        raw-tag-values))
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop)
          (test '() (taglib:raw-tag-property file prop)))
        raw-tag-properties)))))

(test-group "Writing and saving raw tag properties"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (set! (taglib:raw-tag-property file prop) value)
          (test value (taglib:raw-tag-property file prop)))
        raw-tag-properties
        raw-tag-values)
       (taglib:file-save! file))
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (test value (taglib:raw-tag-property file prop)))
        raw-tag-properties
        raw-tag-values)))))

(test-group "Writing and clearing raw tag properties"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (set! (taglib:raw-tag-property file prop) value))
        raw-tag-properties
        raw-tag-values)
       (taglib:file-save! file))
     (let ((file (taglib:file-open path)))
       (for-each
        (lambda (prop value)
          (test value (taglib:raw-tag-property file prop))
          (taglib:raw-tag-property-clear! file prop)
          (test '() (taglib:raw-tag-property file prop)))
        raw-tag-properties
        raw-tag-values)))))

(test-group "Writing raw tag properties in bulk"
  (call-with-test-files
   (lambda (path)
     (let ((file (taglib:file-open path))
           (properties (map cons raw-tag-properties raw-tag-values)))
       (set! (taglib:raw-tag-properties file) properties)
       (test-assert (lset= equal? properties (taglib:raw-tag-properties file)))))))

(test-exit)
