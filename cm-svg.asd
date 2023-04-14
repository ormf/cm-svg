;;; cm-svg.asd
;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.

(asdf:defsystem #:cm-svg
  :description "cm-svg is a backend for common music which exports to
  and imports from svg."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the Lisp LLGPL. See
  http://www.cliki.net/LLGPL for the text of this agreement."
  :depends-on (#:svg-import-export
               #:cm)
  :serial t
  :components ((:file "cm-svg-classes")
               (:file "svg-ie-add-ons")
               (:file "cm-svg")))
