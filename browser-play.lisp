;;; 
;;; browser-play.lisp
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

(in-package :cm)

(defun trim-region (obj &optional (start 0) end)
  (let ((seq (sort (if (typep obj 'cm::container) (subobjects obj) obj)
                   #'< :key #'object-time)))
    (loop until (or (not seq) (>= (object-time (first seq)) start))
          do (setf seq (cdr seq)))
    (loop
      with start-offs = (object-time (first seq))
      for x in seq
      if (or (not end) (<= (object-time x) end)) collect
                                                 (let ((new (copy-object x)))
                                                   (decf (object-time new) start-offs)
                                                   new))))

;;; (chan->color 4)

(defun beat->time (beat &key (factor 4/10))
  (if beat (* beat factor)))

(defun time->beat (time &key (factor 4/10))
  (if time (/ time factor)))

;;; (chan->color 4)

(defun get-first-in-region (evts region timescale)
  (loop
    for e in evts
    until (>= (object-time e) (beat->time (* 16 (- (first region) 1)) :factor timescale))
    finally (return (time->beat (object-time e) :factor timescale))))

(defun region-play (name &key (timescale 1) (region '(0 nil)) (durfac 1) (pstretchfn (lambda (x) (declare (ignore x)) 1)))
  (let* ((obj (import-events (format nil "grafik/~a.svg" name) :x-scale timescale))
         (evts (if obj (sort (subobjects obj) #'< :key #'object-time)))
         (offs (get-first-in-region evts region timescale)))
    (if evts
        (progn
          (free-all-voices)
          (sprout (mapcar (lambda (evt) (let ((keynum (sv evt :keynum)))
                                     (sv* evt :duration durfac)
                                     (setf (sv evt :keynum) (+ 36 (* (- keynum 36)
                                                                     (funcall pstretchfn (1+ (* 1/16 (/ (object-time evt) timescale)))))))
                                     evt))
                          (apply #'trim-region evts
                                 (mapcar (lambda (x) (if x (beat->time (* 16 (- x 1)) :factor timescale))) region))))
          (browser-play offs :tscale timescale))
        (error "obj ~a not found!" (format nil "~a-seq" name)))))

(defparameter *browser-playing* nil)
(defparameter *px-scale* -53)

;;; (defparameter *px-scale* -34.675)

(defun get-tscale (tempo)
  (/ 15/4 (apply #'* tempo)))

(defun browser-play (pos &key (tscale (get-tscale '(1/4 60))))
  "play from pos"
  (setf *browser-playing* t)
  (let* ((sixteensperframe (/ -1/60 tscale))
         (curr (* -1 pos)))
    (labels ((inner (time)
               (when *browser-playing*
                 (let ((next (+ time 1/60)))
                   (cl-user::broadcast-message (format nil "~apx" (incf curr sixteensperframe)))
                   (at next #'inner next)))))
      (inner (now)))))

;;; (browser-play 1000 :tscale (get-tscale '(1 200)))

(defun browser-locate (pos &key (px-scale *px-scale*))
  (cl-user::broadcast-message (format nil "~apx,0" (* pos px-scale))))

;;; (browser-locate 64 :px-scale -1)


(defun browser-stop ()
  (setf *browser-playing* nil))

;;; (browser-stop)
