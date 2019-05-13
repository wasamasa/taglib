(module taglib
  (file-open file-valid? file-save! file-free!
   audio-property audio-properties
   tag-property tag-property-set! tag-properties
   raw-tag-property-exists? raw-tag-property raw-tag-property-clear! raw-tag-property-set! raw-tag-properties raw-tag-properties-set!)

  (import scheme)
  (import (chicken base))
  (import (chicken foreign))
  (import (chicken format))
  (import (chicken gc))
  (import (chicken memory))

  (include "taglib-impl.scm"))
