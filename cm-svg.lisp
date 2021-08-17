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

(defparameter *svg-fn-assoc* nil)

(defun svg-symbol->fn (sym)
  "retrieve the function object from sym. We can't use
#'symbol-function as the package of the function is unspecified in the svg."
  (cdr
   (assoc sym *svg-fn-assoc*)))

(defun add-svg-assoc-fns (fn-assoc-seq)
  (mapc (lambda (fn-assoc)
          (remove-svg-assoc-fn (first fn-assoc))
          (pushnew fn-assoc *svg-fn-assoc* :test #'equal :key #'first))
        fn-assoc-seq)
  *svg-fn-assoc*)

(defun remove-svg-assoc-fn (sym)
  (setf *svg-fn-assoc* (delete-if (lambda (assoc) (eql assoc sym)) *svg-fn-assoc* :key #'first))
  *svg-fn-assoc*)

;;; (remove-svg-assoc-fn 'midi)

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
    (if (event-stream-args io) (warn "non existent keywords for svg-file: ~{:~a~^, ~}
use one of :global :piano-roll-vis :staff-system-vis :bar-lines-vis :showgrid :x-scale :barstepsize :startbar :barmultiplier :width"
                                     (loop for x in (event-stream-args io) by #'cddr collect x)))
    (let ((globs (svg-file-global io)))
      (setf (svg-file-events io) '())
      (if (not (consp globs))
          (setf (svg-file-global io)
                (if (null globs) (list) (list globs))))
      (setf (io-open io) t)))
  io)

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

;;; close-io is called after all events have been collected. This
;;; method actually writes the file."

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
       :bar-lines-vis (bar-lines-vis io)
       :showgrid (showgrid io)
       :x-scale (x-scale io)
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
       do (setf (gethash (string-upcase elem) color-hash) idx))
    color-hash))

(defparameter *svg-colormap*
  (make-colormap
   #("#000000" "#800000" "#FF0000" "#808000" "#FFFF00" "#008000" "#00FF00"
     "#008080" "#00FFFF" "#000080" "#0000FF" "#800080" "#FF00FF" "#AA0000"
     "#280B0B" "#501616" "#782121" "#A02C2C" "#483737" "#6C5353" "#552200"
     "#803300" "#AA4400" "#D45500" "#FF6600" "#002255" "#003380" "#0044AA"
     "#28170B" "#502D16" "#784421" "#A05A2C" "#C87137" "#483E37" "#917C6F"
     "#6C5D53" "#806600" "#AA8800" "#D4AA00" "#112B00" "#225500" "#338000"

     "#000001" "#800001" "#FF0001" "#808001" "#FFFF01" "#008001" "#00FF01"
     "#008081" "#010000" "#000081" "#000100" "#800081" "#FF0100" "#AA0001"
     "#280B0C" "#501617" "#782122" "#A02C2D" "#483738" "#6C5354" "#552201"
     "#803301" "#AA4401" "#D45501" "#FF6601" "#002256" "#003381" "#0044AB"
     "#28170C" "#502D17" "#784422" "#A05A2D" "#C87138" "#483E38" "#917C70"
     "#6C5D54" "#806601" "#AA8801" "#D4AA01" "#112B01" "#225501" "#338001"

     "#000002" "#800002" "#FF0002" "#808002" "#FFFF02" "#008002" "#00FF02"
     "#008082" "#010001" "#000082" "#000101" "#800082" "#FF0101" "#AA0002"
     "#280B0D" "#501618" "#782123" "#A02C2E" "#483739" "#6C5355" "#552202"
     "#803302" "#AA4402" "#D45502" "#FF6602" "#002257" "#003382" "#0044AC"
     "#28170D" "#502D18" "#784423" "#A05A2E" "#C87139" "#483E39" "#917C71"
     "#6C5D55" "#806602" "#AA8802" "#D4AA02" "#112B02" "#225502" "#338002"

     "#000003" "#800003" "#FF0003" "#808003" "#FFFF03" "#008003" "#00FF03"
     "#008083" "#010002" "#000083" "#000102" "#800083" "#FF0102" "#AA0003"
     "#280B0E" "#501619" "#782124" "#A02C2F" "#48373A" "#6C5356" "#552203"
     "#803303" "#AA4403" "#D45503" "#FF6603" "#002258" "#003383" "#0044AD"
     "#28170E" "#502D19" "#784424" "#A05A2F" "#C8713A" "#483E3A" "#917C72"
     "#6C5D56" "#806603" "#AA8803" "#D4AA03" "#112B03" "#225503" "#338003"

     "#000004" "#800004" "#FF0004" "#808004" "#FFFF04" "#008004" "#00FF04"
     "#008084" "#010003" "#000084" "#000103" "#800084" "#FF0103" "#AA0004"
     "#280B0F" "#50161A" "#782125" "#A02C30" "#48373B" "#6C5357" "#552204"
     "#803304" "#AA4404" "#D45504" "#FF6604" "#002259" "#003384" "#0044AE"
     "#28170F" "#502D1A" "#784425" "#A05A30" "#C8713B" "#483E3B" "#917C73"
     "#6C5D57" "#806604" "#AA8804" "#D4AA04" "#112B04" "#225504" "#338004"

     "#000005" "#800005" "#FF0005" "#808005" "#FFFF05" "#008005" "#00FF05"
     "#008085" "#010004" "#000085" "#000104" "#800085" "#FF0104" "#AA0005"
     "#280B10" "#50161B" "#782126" "#A02C31" "#48373C" "#6C5358" "#552205"
     "#803305" "#AA4405" "#D45505" "#FF6605" "#00225A" "#003385" "#0044AF"
     "#281710" "#502D1B" "#784426" "#A05A31" "#C8713C" "#483E3C" "#917C74"
     "#6C5D58" "#806605" "#AA8805" "#D4AA05" "#112B05" "#225505" "#338005"

     "#000006" "#800006" "#FF0006" "#808006" "#FFFF06" "#008006" "#00FF06"
     "#008086" "#010005" "#000086" "#000105" "#800086" "#FF0105" "#AA0006"
     "#280B11" "#50161C" "#782127" "#A02C32" "#48373D" "#6C5359" "#552206"
     "#803306" "#AA4406" "#D45506" "#FF6606" "#00225B" "#003386" "#0044B0"
     "#281711" "#502D1C" "#784427" "#A05A32" "#C8713D" "#483E3D" "#917C75"
     "#6C5D59" "#806606" "#AA8806" "#D4AA06" "#112B06" "#225506" "#338006"

     "#000007" "#800007" "#FF0007" "#808007" "#FFFF07" "#008007" "#00FF07"
     "#008087" "#010006" "#000087" "#000106" "#800087" "#FF0106" "#AA0007"
     "#280B12" "#50161D" "#782128" "#A02C33" "#48373E" "#6C535A" "#552207"
     "#803307" "#AA4407" "#D45507" "#FF6607" "#00225C" "#003387" "#0044B1"
     "#281712" "#502D1D" "#784428" "#A05A33" "#C8713E" "#483E3E" "#917C76"
     "#6C5D5A" "#806607" "#AA8807" "#D4AA07" "#112B07" "#225507" "#338007"

     "#000008" "#800008" "#FF0008" "#808008" "#FFFF08" "#008008" "#00FF08"
     "#008088" "#010007" "#000088" "#000107" "#800088" "#FF0107" "#AA0008"
     "#280B13" "#50161E" "#782129" "#A02C34" "#48373F" "#6C535B" "#552208"
     "#803308" "#AA4408" "#D45508" "#FF6608" "#00225D" "#003388" "#0044B2"
     "#281713" "#502D1E" "#784429" "#A05A34" "#C8713F" "#483E3F" "#917C77"
     "#6C5D5B" "#806608" "#AA8808" "#D4AA08" "#112B08" "#225508" "#338008"

     "#000009" "#800009" "#FF0009" "#808009" "#FFFF09" "#008009" "#00FF09"
     "#008089" "#010008" "#000089" "#000108" "#800089" "#FF0108" "#AA0009"
     "#280B14" "#50161F" "#78212A" "#A02C35" "#483740" "#6C535C" "#552209"
     "#803309" "#AA4409" "#D45509" "#FF6609" "#00225E" "#003389" "#0044B3"
     "#281714" "#502D1F" "#78442A" "#A05A35" "#C87140" "#483E40" "#917C78"
     "#6C5D5C" "#806609" "#AA8809" "#D4AA09" "#112B09" "#225509" "#338009")))

#|
(remove-duplicates
 (let ((colors '("#000000" "#800000" "#ff0000" "#808000" "#ffff00" "#008000" "#00ff00"
                 "#008080" "#00ffff" "#000080" "#0000ff" "#800080" "#ff00ff" "#aa0000"
                 "#280b0b" "#501616" "#782121" "#a02c2c" "#483737" "#6c5353" "#552200"
                 "#803300" "#aa4400" "#d45500" "#ff6600" "#002255" "#003380" "#0044aa"
                 "#28170b" "#502d16" "#784421" "#a05a2c" "#c87137" "#483e37" "#917c6f"
                 "#6c5d53" "#806600" "#aa8800" "#d4aa00" "#112b00" "#225500" "#338000")))
   (loop for count below 10
      append (loop for color in colors collect (format nil "#~6,'0x" (+ count (read-from-string (format nil "#x~a" (subseq color 1))))))
        ))
 :test #'string=)
|#

(defun chan->color (midi-chan &optional (colormap *svg-colormap*))
  "rgb color lookup for the first 16 MIDI channels."
  (aref (gethash :vector colormap)
        (mod midi-chan (length (gethash :vector colormap)))))

(defun color->chan (color &optional (colormap *svg-colormap*))
  (or (and colormap (gethash (string-upcase color) colormap)) 0))

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
                 (make-instance 'svg-ie::svg-line :x1 (float x1) :y1 (float y1)
                                :x2 (float (+ x1 width)) :y2 (float y1)
                                :stroke-width stroke-width
                                :opacity opacity
                                :stroke-color color 
                                ;; :fill-color color
                                :id (new-id fil 'line-ids)))))
    (svg-file-insert-line line myid fil)))

(defun recreate-from-attributes (args)
  "recreate a cm object according to the :attributes property of the
svg element."
  (apply (svg-symbol->fn (getf args :type)) (progn (remf args :type) args)))

(defun svg->midi (&rest args)
  (apply #'make-instance 'midi
         (ou:get-props-list args '(:time :keynum :duration :amp :channel))))

(add-svg-assoc-fns `((midi . ,(symbol-function 'svg->midi))))

#|
(defun get-props-list (attributes &rest props)
  (reduce (lambda (seq prop) (let ((val (getf attributes prop :not-supplied)))
                          (if (eql val :not-supplied)
                              seq
                              (list* prop val seq))))
          (reverse props)
          :initial-value nil))

;;; (get-props-list '(:amp 2 :dur 4 :time 12 :keynum 30 :hallo nil) :amp :dur :hallo)
|#

(defun keynum->pitch (args)
  "prevent shadowing of :keynum in args by renaming it to :pitch"
  (setf (getf args :pitch) (getf args :keynum))
  (remf args :keynum)
  args)

(defun svg->cm (file layer x-scale &key colormap start end)
  (let* ((x-offs (if start (* -1 (/ start x-scale)) 0))
         (ende (if end (+ x-offs (/ end x-scale)) most-positive-fixnum)))
;;;    (break "x-offs: ~a ende: ~a" x-offs ende)
    (mapcar
     (lambda (line)
       (ou:with-props (x1 y1 x2 color opacity attributes) line
         (when (not attributes) (setf (getf attributes :type) 'midi))
         (recreate-from-attributes (list* :time (float (* x-scale x1))
                                          :keynum y1
                                          :duration (float (* x-scale (- x2 x1)))
                                          :amplitude opacity
                                          :channel (color->chan color colormap)
                                          (keynum->pitch attributes)))))
     (remove-if-not (lambda (line) (<= 0 (getf line :x1) ende))
                    (svg-ie::svg->lines :infile file :layer layer :xquantize nil :yquantize nil :x-offset x-offs)))))


(defmethod import-events ((file svg-file) &key (seq t) layer (x-scale 1/32) (colormap *svg-colormap*) (start 0) end)
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
    (let ((events (svg->cm fil (or layer "Events") x-scale :colormap colormap :start start :end end)))
      (if (and seq events)
          (progn (setf (container-subobjects seq) events)
                 seq)
          events))))




(export '(*SVG-COLORMAP* COLOR->CHAN CHAN->COLOR ADD-RECREATION-FN) 'cm)
