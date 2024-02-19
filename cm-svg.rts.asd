;;; cm-svg.rts.asd
;;;
;;; rts extensions to cm-svg
;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.

(asdf:defsystem #:cm-svg.rts
  :description "cm-svg.rts contains extesnions for realtime playback
  supports playing selections from inkscape using the \"Play Selection\" inkscape plugin."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the Lisp LLGPL. See
  http://www.cliki.net/LLGPL for the text of this agreement."
  :depends-on (#:svg-import-export #:incudine #:of-incudine-dsps
               #:cl-refs #:cm-svg #:clog-dsp-widgets)
  :serial t
  :components ((:file "package")
               (:file "inkscape-export")
               (:file "display-automation")))
