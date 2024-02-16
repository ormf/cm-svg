;;; cm-svg-classes.lisp
;;; 
;;; class definitions of svg-file and assignment of ".svg" file type
;;; for the events function.
;;;
;;; **********************************************************************
;;; Copyright (C) 2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package :cm)

(progn
  (defclass svg-file (event-file)
    ((events :initarg :events :initform '() :accessor svg-file-events) ;;; the events to export
     (global :initarg :global :initform '() :accessor svg-file-global) ;;; unused optional global params
     (id-hash :initarg :id-hash :initform (make-hash-table) :accessor svg-file-id-hash) ;;; hashtable for the ids of svg elements
     (iinverse :initarg :inverse :initform nil :accessor svg-file-inverse) ;;; inverted colors (black backgound)
     (view :initarg :view :initform t :accessor svg-file-view)
     (width :initarg :width :initform nil :accessor width)
     (x-scale :initarg :x-scale :initform 32 :accessor x-scale) ;;; x-scale of events: 1 is a grid-point in the svg
     (y-offs :initarg :y-offs :initform 0 :accessor y-offs) ;;; y-offs of events: 1 is a grid-point in the svg
     (piano-roll-vis :initarg :piano-roll-vis :initform t :accessor piano-roll-vis) ;;; visibility flag for piano roll layer
     (staff-system-vis :initarg :staff-system-vis :initform t :accessor staff-system-vis)
     (bar-lines-vis :initarg :bar-lines-vis :initform t :accessor bar-lines-vis) ;;; visibility flag for bar lines layer
     (barstepsize :initarg :barstepsize :initform 1 :accessor barstepsize)
     (startbar :initarg :startbar :initform 1 :accessor startbar)
     (barmultiplier :initarg :barmultiplier :initform 4 :accessor barmultiplier)
     (timesigs :initarg :timesigs :initform nil :accessor timesigs)
     (zoom :accessor zoom :initarg :zoom :initform 1.4)
     (cx :accessor cx :initarg :cx :initform 350)
     (cy :accessor cy :initarg :cy :initform 360)
     (w-width :accessor w-width :initarg :w-width :initform 1920)
     (w-height :accessor w-height :initarg :w-height :initform 1080)
     (w-x :accessor w-x :initarg :w-x :initform 0)
     (w-y :accessor w-y :initarg :w-y :initform 0)
     (showgrid :initarg :showgrid :initform t :accessor showgrid) ;;; visibility flag for grid
     (gridtype :initarg :gridtype :initform "4x4" :accessor gridtype)
     (expand :initarg :expand :initform t :accessor expand))
    #+metaclasses
    (:metaclass io-class))
  (defparameter <svg-file> (find-class 'svg-file))
  (finalize-class <svg-file>)
  (setf (io-class-file-types <svg-file>)
        '("*.svg"))
  (values))
