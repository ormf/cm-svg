;;; cm-svg.lisp
;;; 
;;; Code for the support of .svg for common music events
;;; function. Currently only midi is supported as output format.
;;;
;;; **********************************************************************
;;; Copyright (C) 2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package :cm)

(defun new-id (svg-file id-type)
  "return a new id of the specified id-type by incrementing a counter
stored in a hash table with id-type as keys."
  (incf (gethash id-type (svg-file-id-hash svg-file) 0)))

#|
(let ((file (make-instance 'svg-file)))
  (new-id file 'group-ids))
|#

;;; initialize an svg-file instance by setting the events to nil and
;;; (re)setting the global slot.

(defmethod open-io ((io svg-file) dir &rest args)
  args
  (when (eq dir ':output)
    (let ((globs (svg-file-global io)))
      (setf (svg-file-events io) '())
      (if (not (consp globs))
          (setf (svg-file-global io)
                (if (null globs) (list) (list globs))))
      (setf (io-open io) t)))
  io)

;;; close-io is called, after all events have been collected. This
;;; method actually writes the file."

(defgeneric endtime (obj))

(defmethod endtime ((obj midi))
  (+ (sv obj :time) (midi-duration obj)))

(defmethod endtime ((obj svg-ie:svg-layer)) 0)
(defmethod endtime ((obj svg-ie::svg-tl-layer)) 0)
(defmethod endtime ((obj svg-ie::svg-line)) (sv obj svg-ie:x2))
(defmethod endtime ((obj svg-ie::svg-group)) 0)

(defgeneric total-duration (events))

(defmethod endtime ((seq seq))
  (apply #'max (mapcar #'endtime (subobjects seq))))

(defmethod endtime ((seq list))
  (apply #'max (mapcar #'endtime seq)))

(defmethod close-io ((io svg-file) &rest mode)
  (let ((err? (and (not (null mode)) (eq (car mode) ':error))))
    (setf (io-open io) nil)
    (unless err?
      (cm-svg-export
       :fname (sv io :name)
       :events (svg-file-events io)
       :global (svg-file-global io)
       :piano-roll-vis (piano-roll-vis io)
       :staff-system-vis (staff-system-vis io)
       :showgrid (showgrid io)
       :x-scale (x-scale io)
       :bar-lines-vis (bar-lines-vis io)
       :barstepsize (barstepsize io)
       :startbar (startbar io)
       :barmultiplier (barmultiplier io)
       :width (or (width io) (endtime (svg-file-events io)))))))

;;; cm-svg-export initializes a svg-ie:svg-file instance, fills
;;; its elements slot with the svg-objects of staff-system,
;;; piano-roll, and the collected events. The events have been
;;; collected by the #'write-event method into the events slot of the
;;; svg-file object defined below. write-event collects the events
;;; into a list with one sublist for each midi-channel. The sublists
;;; contain a svg-ie:layer object as the first element and
;;; svg-ie:line objects for each MIDI event in that channel. As
;;; write-event pushes the line objects into the cdr of the list
;;; (after the layer object), the cdrs of the channel lists are
;;; reversed and the list with all layer channels gets sorted by MIDI
;;; channel number before calling svg-ie:export-svg-file.

(defun cm-svg-export (&key events global (staff-system-vis t) (piano-roll-vis t) (fname "/tmp/test.svg")
                        (showgrid t) (width 10000) (x-scale 8) (bar-lines-vis t) (barstepsize 4) (startbar 1) (barmultiplier 1)
                        &allow-other-keys)
  (declare (ignore global))
  (let ((svg-file (make-instance 'svg-ie:svg-file)))
    (setf (svg-ie::elements svg-file)
          (append
           (list (svg-ie:svg-staff-system svg-file :visible staff-system-vis :width width))
           (list (svg-ie:svg-piano-roll svg-file :visible piano-roll-vis :width width))
           (list (svg-ie:svg-barlines svg-file :visible bar-lines-vis :width width :x-scale x-scale
                                      :barstepsize barstepsize :startbar startbar :barmultiplier barmultiplier))
           (list (cons (make-instance 'svg-ie::svg-tl-layer :name "Events" :id "ebenen-id")
                       (sort (mapcar (lambda (chan) (cons (first chan) (reverse (rest chan)))) events)
                             #'string> :key (lambda (x) (slot-value (car x) 'svg-ie::name)))))))
    (svg-ie:export-svg-file svg-file :fname fname :showgrid showgrid :width width)))

(defun chan-eq? (chan layer-obj)
  "check if chan matches the ch<chan> in the name (label) of the
layer-obj. Note the offset by one as the label counts MIDI channels
from 1."
  (string= (format nil "ch~2,'0d" (1+ chan))
           (slot-value layer-obj 'svg-ie::name)))

(defun svg-file-insert-line (line id stream)
  "insert an svg-ie:svg-line object at the appropriate position of
the elements slot."
  (do ((tail (svg-file-events stream) (cdr tail))) ;;; cdr over events list of svg-file
      ((or (null tail) ;;; channel list not yet existing
           (chan-eq? id (caar tail))) ;;; matching channel list found
       (if tail ;;; matching channel list found
         (push line (cdar tail)) ;;; push line into list after the layer object (the first element of tail)
         (setf (svg-file-events stream) ;;; cons new channel list with layer object and line to the events list.
               (cons (list (make-instance 'svg-ie::svg-layer :name (format nil "ch~2,'0d" (1+ id))
                                          :id (new-id stream 'layer-ids))
                           line)
                     (svg-file-events stream)))))))

(defun make-colormap (vector)
  (let* ((color-hash (make-hash-table :test #'equal)))
    (setf (gethash :vector color-hash) vector)
    (loop for idx from 0 for elem across vector
       do (setf (gethash elem color-hash) idx))
    color-hash))

(defparameter *svg-colormap*
  (make-colormap
   #("#000000" "#800000" "#ff0000" "#808000" "#ffff00" "#008000" "#00ff00"
     "#008080" "#00ffff" "#000080" "#0000ff" "#800080" "#ff00ff" "#aa0000"
     "#280b0b" "#501616" "#782121" "#a02c2c" "#483737" "#6c5353" "#552200"
     "#803300" "#aa4400" "#d45500" "#ff6600" "#002255" "#003380" "#0044aa"
     "#28170b" "#502d16" "#784421" "#a05a2c" "#c87137" "#483e37" "#917c6f"
     "#6c5d53" "#806600" "#aa8800" "#d4aa00" "#112b00" "#225500" "#338000")))

(defun chan->color (midi-chan &optional (colormap *svg-colormap*))
  "rgb color lookup for the first 16 MIDI channels."
  (aref (gethash :vector colormap)
        midi-chan))

(defun color->chan (color &optional (colormap *svg-colormap*))
  (or (and colormap (gethash color colormap)) 0))

(defmethod write-event ((obj midi) (fil svg-file) scoretime)
  "convert a midi object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (let* ((myid (midi-channel obj))
         (x-scale (x-scale fil))
         (stroke-width 0.5)
         (line (let ((x1 (* x-scale scoretime))
                     (y1 (* 1 (midi-keynum obj)))
                     (width (* x-scale (midi-duration obj)))
                     (color (chan->color myid))
                     (opacity (midi-amplitude obj)))
                 (make-instance 'svg-ie::svg-line :x1 x1 :y1 y1
                                :x2 (+ x1 width) :y2 y1
                                :stroke-width stroke-width
                                :opacity opacity
                                :stroke-color color 
                                ;; :fill-color color
                                :id (new-id fil 'line-ids)))))
    (svg-file-insert-line line myid fil)))

(defun svg->midi (file layer x-scale &key colormap)
    (mapcar
     (lambda (line) (destructuring-bind (time keynum dur color amp) line
                 (new midi :time (float (* x-scale time)) :keynum keynum :duration (float (* x-scale dur)) :amplitude amp
                      :channel (color->chan color colormap))))
     (svg-ie::svg->lines :infile file :layer layer :xquantize nil :yquantize nil)))

(defmethod import-events ((file svg-file) &key (seq t) layer (x-scale 1) colormap)
  (let ((fil (file-output-filename file)))
    (cond ((or (not seq) (typep seq <seq>)) nil)
          ((eq seq t)
           (setf seq
                 (make-instance
                   <seq>
                   :name
                   (format nil "~a~a-seq" (filename-name fil)
                           (if layer (format nil "-~a" layer) "")))))
          (t
           (error "import-events: ~S is not a boolean or seq." seq)))
    (let ((events (svg->midi fil (or layer "Events") x-scale :colormap colormap)))
      (if (and seq events)
          (progn (setf (container-subobjects seq) events)
                 seq)
          events))))

(export '*SVG-COLORMAP* 'cm)
(export 'color->chan 'cm)
(export 'chan->color 'cm)
