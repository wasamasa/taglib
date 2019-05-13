;;; headers

#> #include "taglib.h" <#

;;; typedefs

(define-foreign-type TagLib_File* (instance "TagLib::File" 'taglib-file))

;;; auxiliary records

(define (format-pointer pointer)
  (if pointer
      (format "0x~x" (pointer->address pointer))
      "NULL"))

(define-record taglib-file pointer)
(define-record-printer (taglib-file f out)
  (fprintf out "#<taglib-file: ~a>" (format-pointer (taglib-file-pointer f))))

;;; class helpers

(define (make type _slot pointer)
  (case type
    ((taglib-file) (make-taglib-file pointer))
    (else (error "Unknown C++ type"))))

(define (slot-ref thing _slot)
  (cond
   ((taglib-file? thing) (taglib-file-pointer thing))
   (else (error "Unknown C++ type"))))

;;; foreign functions

(define taglib_new (foreign-lambda TagLib_File* "taglib_new" c-string))
(define taglib_is_valid (foreign-lambda bool "taglib_is_valid" TagLib_File*))
(define taglib_save (foreign-lambda bool "taglib_save" TagLib_File*))
(define taglib_free (foreign-lambda void "taglib_free" TagLib_File*))
(define taglib_length (foreign-lambda int "taglib_length" TagLib_File*))
(define taglib_bitrate (foreign-lambda int "taglib_bitrate" TagLib_File*))
(define taglib_samplerate (foreign-lambda int "taglib_samplerate" TagLib_File*))
(define taglib_channels (foreign-lambda int "taglib_channels" TagLib_File*))
(define taglib_title (foreign-lambda c-string* "taglib_title" TagLib_File*))
(define taglib_artist (foreign-lambda c-string* "taglib_artist" TagLib_File*))
(define taglib_album (foreign-lambda c-string* "taglib_album" TagLib_File*))
(define taglib_comment (foreign-lambda c-string* "taglib_comment" TagLib_File*))
(define taglib_genre (foreign-lambda c-string* "taglib_genre" TagLib_File*))
(define taglib_year (foreign-lambda unsigned-int "taglib_year" TagLib_File*))
(define taglib_track (foreign-lambda unsigned-int "taglib_track" TagLib_File*))
(define taglib_title_set (foreign-lambda void "taglib_title_set" TagLib_File* c-string))
(define taglib_artist_set (foreign-lambda void "taglib_artist_set" TagLib_File* c-string))
(define taglib_album_set (foreign-lambda void "taglib_album_set" TagLib_File* c-string))
(define taglib_comment_set (foreign-lambda void "taglib_comment_set" TagLib_File* c-string))
(define taglib_genre_set (foreign-lambda void "taglib_genre_set" TagLib_File* c-string))
(define taglib_year_set (foreign-lambda void "taglib_year_set" TagLib_File* unsigned-int))
(define taglib_track_set (foreign-lambda void "taglib_track_set" TagLib_File* unsigned-int))
(define taglib_raw_property_exists (foreign-lambda bool "taglib_raw_property_exists" TagLib_File* c-string))
(define taglib_raw_property_keys (foreign-lambda c-string-list* "taglib_raw_property_keys" TagLib_File*))
(define taglib_raw_property_ref (foreign-lambda c-string-list* "taglib_raw_property_ref" TagLib_File* c-string))
(define taglib_raw_property_clear (foreign-lambda void "taglib_raw_property_clear" TagLib_File* c-string))
(define taglib_raw_property_set (foreign-lambda void "taglib_raw_property_set" TagLib_File* c-string scheme-object))
(define taglib_raw_properties_set (foreign-lambda c-string-list* "taglib_raw_properties_set" TagLib_File* scheme-object))

;;; API

(define (file-free! file)
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_free file*)
    (taglib-file-pointer-set! file #f)))

(define (file-open path)
  (and-let* ((file* (taglib_new path)))
    (set-finalizer! (make-taglib-file file*) file-free!)))

(define (file-valid? file)
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_is_valid file*)))

(define (file-save! file)
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_save file*)))

(define (non-zero-number number)
  (if (= number 0)
      #f
      number))

(define (audio-property file key)
  (and-let* ((file* (taglib-file-pointer file)))
    (case key
      ((length) (non-zero-number (/ (taglib_length file*) 1000)))
      ((bitrate) (non-zero-number (taglib_bitrate file*)))
      ((samplerate) (non-zero-number (taglib_samplerate file*)))
      ((channels) (non-zero-number (taglib_channels file*)))
      (else (error "Unknown property" key)))))

(define (audio-properties file)
  (and-let* ((file* (taglib-file-pointer file)))
    `((length . ,(non-zero-number (/ (taglib_length file*) 1000)))
      (bitrate . ,(non-zero-number (taglib_bitrate file*)))
      (samplerate . ,(non-zero-number (taglib_samplerate file*)))
      (channels . ,(non-zero-number (taglib_channels file*))))))

(define (tag-property file key)
  (and-let* ((file* (taglib-file-pointer file)))
    (case key
      ((title) (taglib_title file*))
      ((artist) (taglib_artist file*))
      ((album) (taglib_album file*))
      ((comment) (taglib_comment file*))
      ((genre) (taglib_genre file*))
      ((year) (non-zero-number (taglib_year file*)))
      ((track) (non-zero-number (taglib_track file*)))
      (else (error "Unknown property" key)))))

(define (tag-property-set! file key value)
  (and-let* ((file* (taglib-file-pointer file)))
    (case key
      ((title) (taglib_title_set file* value))
      ((artist) (taglib_artist_set file* value))
      ((album) (taglib_album_set file* value))
      ((comment) (taglib_comment_set file* value))
      ((genre) (taglib_genre_set file* value))
      ((year) (taglib_year_set file* (or value 0)))
      ((track) (taglib_track_set file* (or value 0)))
      (else (error "Unknown property" key)))))

(define tag-property
  (getter-with-setter tag-property tag-property-set!))

(define (tag-properties file)
  (and-let* ((file* (taglib-file-pointer file)))
    `((title . ,(taglib_title file*))
      (artist . ,(taglib_artist file*))
      (album . ,(taglib_album file*))
      (comment . ,(taglib_comment file*))
      (genre . ,(taglib_genre file*))
      (year . ,(non-zero-number (taglib_year file*)))
      (track . ,(non-zero-number (taglib_track file*))))))

(define (raw-tag-property-exists? file key)
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_raw_property_exists file* key)))

(define (raw-tag-property file key)
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_raw_property_ref file* key)))

(define (raw-tag-property-clear! file key)
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_raw_property_clear file* key)))

(define (string-list? arg)
  (and (pair? arg) (not (member #f (map string? arg)))))

(define (string-lists? arg)
  (and (list? arg) (not (member #f (map string-list? arg)))))

(define (raw-tag-property-set! file key values)
  (when (not (string-list? values))
    (error "Not a string list" values))
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_raw_property_set file* key values)))

(define raw-tag-property
  (getter-with-setter raw-tag-property raw-tag-property-set!))

(define (raw-tag-properties file)
  (and-let* ((file* (taglib-file-pointer file)))
    (map (lambda (key)
           (cons key (taglib_raw_property_ref file* key)))
         (taglib_raw_property_keys file*))))

(define (raw-tag-properties-set! file properties)
  (when (not (string-lists? properties))
    (error "Not a list of string lists" properties))
  (and-let* ((file* (taglib-file-pointer file)))
    (taglib_raw_properties_set file* properties)))

(define raw-tag-properties
  (getter-with-setter raw-tag-properties raw-tag-properties-set!))
