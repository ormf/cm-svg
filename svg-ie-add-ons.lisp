;;; 
;;; svg-ie-add-ons.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :svg-import-export)

(defparameter *svg-attr-props-to-quote* nil)

(defun add-svg-attr-props-to-quote (&rest props)
  (mapc (lambda (prop) (pushnew prop *svg-attr-props-to-quote*)) props)
  *svg-attr-props-to-quote*)

(defun quote-svg-attr-prop (attr-str prop)
  (multiple-value-bind (str reg)
      (cl-ppcre:scan-to-strings ;;; this voodoo is to catch spaces in property value strings
       (format nil "\(^.\*~S\) \+\([^:]\+\)\(.\*\)$" prop)
       attr-str)
    (declare (ignore str))
    (if reg
        (format nil "~a ~S ~a"
                (aref reg 0)
                (string-right-trim '(#\SPACE) (aref reg 1))
                (aref reg 2))
        attr-str)))

(defun quote-svg-attr-props (str)
  (reduce #'quote-svg-attr-prop *svg-attr-props-to-quote* :initial-value str))

(defclass svg-cm-line ()
  ((x1 :accessor svg-cm-line-x1 :initarg :x1)
   (y1 :accessor svg-cm-line-y1 :initarg :y1)
   (x2 :accessor svg-cm-line-x2 :initarg :x2)
   (y2 :accessor svg-cm-line-y2 :initarg :y2)
   (color :accessor svg-cm-line-color :initarg :color)
   (opacity :accessor svg-cm-line-opacity :initarg :opacity)
   (attributes :accessor svg-cm-line-attributes :initarg :attributes)))

(defun make-cm-line (args)
  "wrapper function for mapping."
  (apply #'make-instance 'svg-cm-line args))

(defun get-path-coords (node parse-state &key (x-offset 0) (timescale 1) (xquantize nil) (yquantize nil))
  "return an svg-cm-line instance from path node."
  (let* ((path (parse-path2 (cxml-stp:value (cxml-stp:find-attribute-named node "d"))))
         (style-string (cxml-stp:value (cxml-stp:find-attribute-named node "style")))
         (attributes (and (cxml-stp:find-attribute-named node "attributes")
                          (cxml-stp:value (cxml-stp:find-attribute-named node "attributes"))))
         (transformation (update-transformation (svg-parse-state-transformation parse-state) node)))
    (if path
        (destructuring-bind ((x1 y1) (x2 y2))
            (sort
             (list (funcall #'vec-mtx-mult (first path) transformation)
                   (funcall #'vec-mtx-mult (second path) transformation))
             #'< :key #'first)
          (make-instance 'svg-cm-line
           :x1
           (if xquantize
               (* (round (* 2 timescale (+ x-offset x1))) 0.5)
               (+ x-offset (* timescale x1)))
           :y1
           (if yquantize
               (round (* 1 y1))
               (* 1 y1))
           :x2
           (if xquantize
                  (round (* timescale x2))
                  (+ x-offset (* timescale x2)))
           :y2
           (if yquantize
               (round (* 1 y2))
               (* 1 y2))
           :color
           (style-stroke-color style-string)
           :opacity
           (* (svg-parse-state-opacity parse-state)
              (style-opacity style-string))
           :attributes (if (and attributes (string/= (string-upcase attributes) "NONE"))
                           (read-from-string (format nil "(~a)" (quote-svg-attr-props attributes))))
           ))
        (warn "~a is empty!" (cxml-stp:value (cxml-stp:find-attribute-named node "id"))))))

(export
 '(MAKE-CM-LINE
   ADD-SVG-ATTR-PROPS-TO-QUOTE
   SVG-CM-LINE
   MAKE-SVG-CM-LINE
   SVG-CM-LINE-X1
   SVG-CM-LINE-X2
   SVG-CM-LINE-Y1
   SVG-CM-LINE-Y2
   SVG-CM-LINE-COLOR
   SVG-CM-LINE-OPACITY
   SVG-CM-LINE-ATTRIBUTE)
 'svg-import-export)
