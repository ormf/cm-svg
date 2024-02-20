;;; 
;;; display-automation.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

;;; (export 'sfz 'cm)

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

#|
(defun free-voice (num)
  (setf *curr-voices* (delete num *curr-voices*))
  (display-send num :vn-nonvib-mf-4c -500))

(defun free-all-voices ()
  (dotimes (n 16)
    (funcall (free-voice n)))
  (setf *curr-voices* nil))
|#

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

(defun seq-play (obj)
  (let* ((evts (subobjects obj))
;;;         (offs (get-first-in-region evts region timescale))
         )
    (if evts
        (progn
          (let ((curr-pos (get-val svg-shift)))
            (dolist (obj evts)
              (when (>= (object-time obj) curr-pos)
                (let ((obj (copy-object obj)))
                  (setf (sv obj :time) (float (* (get-val svg-timescale) (- (sv obj :time) curr-pos))))
                  (sv* obj :duration (get-val svg-timescale))
;;;                  (format t "~a~%" obj)
                  (sprout obj))
                )
              ))
          ;;          (browser-play (* offs 6.041) :tscale (/ 1/8 6.041))
          ))))

(defmacro sv- (obj slot val &body more) (svaux obj '- slot val more))

(seq-play (find-object "hdbg04q-sfz-seq"))


2481.235

(set-val svg-timescale 1/8)

(let ((evts (sort (mapcar #'copy-object (subobjects (get-val svg-seq))) #'< :key #'object-time)))
  (setf (object-time (first evts)) 1000)
  evts
  )


(sort (copy-tree (subobjects (get-val svg-seq))) #'< :key #'object-time)

(region-play (get-val svg-seq) :timescale 15 :region (list (get-val svg-shift) nil))

;;; (use-package :clog)

(progn
  (defparameter cursor-pos nil)
  (defparameter svg-shift nil)
  (defparameter svg-width nil)
  (defparameter svg-scale nil)
  (defparameter svg-seq nil)
  (defparameter svg-timescale nil)
  (defparameter svg-piano-roll nil)
  (defparameter svg-staff-systems nil)
  (defparameter svg-bar-lines nil)
  (defparameter idx nil)
  (defparameter data nil)
  (defparameter transport nil)
  (defparameter auto-return (make-ref 0))
  (defparameter play-watch nil)
  (defparameter data-watch nil))

(progn
  (clear-bindings)
  (when play-watch (funcall play-watch))
  (when data-watch (funcall data-watch))
  (setf cursor-pos (make-ref 0.5))
  (setf svg-shift (make-ref 0))
  (setf svg-seq (make-ref nil))
  (setf svg-width (make-ref 0))
  (setf svg-scale (make-ref 9.5))
  (setf svg-timescale (make-ref 1/8))
  (setf svg-piano-roll (make-ref 1))
  (setf svg-staff-systems (make-ref 1))
  (setf svg-bar-lines (make-ref 1))
  (setf idx (make-ref 0))
  (setf transport (make-ref 0))
  (setf data (make-ref "hdbg04q-sfz.svg"))
  (setf play-watch (watch (let ((last-pos 0))
                            (lambda () (if (zerop (get-val transport))
                                      (let (cl-refs::*curr-ref*)
                                        (format t "stopping~%")
                                        (incudine:node-free-all)
                                        (incudine:flush-pending)
                                        (unless (zerop (get-val auto-return))
                                          (set-val svg-shift last-pos )))
                                      (let (cl-refs::*curr-ref*)
                                        (format t "relocating~%")
                                        (setf last-pos (get-val svg-shift))
                                        (seq-play (get-val svg-seq))
                                        (svg-play)

))))))
  (setf data-watch (watch (lambda ()
;;;                            (format t "reset-seq")
                            (set-val svg-seq
                                     (let ((seq
                                             (cm:import-events
                                              (namestring
                                               (merge-pathnames
                                                (format nil "www/~A" (get-val data))
                                                (asdf:system-source-directory :clog-dsp-widgets)))
                                              :x-scale 1)))
                                       (setf (container-subobjects seq) (sort (subobjects seq) #'< :key #'object-time))
                                       seq)))))
  nil)



;;; (set-val svg-timescale 1/8)


;;; (get-val svg-seq)
;;; (set-val svg-timescale 1)



;;, (set-val data "hdbg04b-sfz.svg")
(set-val data "hdbg04q-sfz.svg")
;;; (set-val svg-scale 9.5)
;;; (get-val svg-seq)
;;; (get-val svg-width)

(defun set-keyboard-shortcuts (container transport-toggle)
  (clog:js-execute
   container
   (format nil "document.onkeydown = function (event) {
  if (event.which == 32 || event.code == 'Space') {
    let transportToggle = document.getElementById('~a'); 
    let currValue = transportToggle.getAttribute('value');
    transportToggle.externalValueChange = false;
    if (currValue == 0) {
      transportToggle.setAttribute('value', 1);
    }
    else {
      transportToggle.setAttribute('value', 0);
    }
  }
};
"
           (clog:html-id transport-toggle))))

(defun new-window (body)
  "On-new-window handler."
  (let (transport-toggle)
    (setf (clog:title (clog:html-document body)) "SVG Test")
    (create-o-svg
     body (bind-refs-to-attrs svg-width "width"
                              cursor-pos "cursor-pos"
                              svg-shift "shift-x"
                              data "data"
                              svg-scale "scale"
                              svg-piano-roll "piano-roll"
                              svg-staff-systems "staff-systems"
                              svg-bar-lines "bar-lines"))
    (create-o-slider body (bind-refs-to-attrs svg-shift "value" svg-width "max")
                     :min 0 :max 200 :direction :right
                     :css `(:display "inline-block" :height "1em" :width "100%"))
    (setf transport-toggle
          (create-o-toggle body (bind-refs-to-attrs transport "value")
                           :label '("play" "stop") :background '("transparent" "#8f8")
                           :css `(:display "inline-block" :height "1.2em" :width "3em")))
    (create-o-toggle body (bind-refs-to-attrs auto-return "value")
                     :label '("rtn") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (create-o-toggle body (bind-refs-to-attrs svg-piano-roll "value")
                     :label '("pno") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (create-o-toggle body (bind-refs-to-attrs svg-staff-systems "value")
                     :label '("stf") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (create-o-toggle body (bind-refs-to-attrs svg-bar-lines "value")
                     :label '("bar") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (set-keyboard-shortcuts body transport-toggle)
    ))



(defun on-new-window (body)
  (new-window body))

;; Initialize the CLOG system with a boot file which contains the
;; static js files. For customized uses copy the "www" subdirectory of
;; the repository to your local project and adjust :static-root
;; accordingly
(defun start-svg-display (&key (static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-dsp-widgets))))
  (clear-bindings) ;;; start from scratch
  (clog:initialize nil
              :port 8080
              :static-root static-root
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (clog:set-on-new-window  #'on-new-window :path "/svg-display" :boot-file "/start.html")
  (clog:open-browser :url "http://127.0.0.1:8080/svg-display"))

;;; (start) should start a webserver with some gui widgets that are
;;; connected

;;; (start-svg-display)

;; (set-val cursor-pos 0.5)
;; (set-val svg-width 8000)

;; (set-val svg-scale 9.5)

;;; (set-val svg-timescale 0.125) 


(defun install-key-shortcuts (container vu-id preset-panel-id)
  (js-execute
   container
   (format nil "document.onkeydown = function (event) {
  if (event.which == 112 || event.keyCode == 112) {
   document.getElementById('~a').style.display = \"flex\";
   document.getElementById('~a').style.display = \"none\";
  }
  if (event.which == 113 || event.keyCode == 113) {
   document.getElementById('~a').style.display = \"none\";
   document.getElementById('~a').style.display = \"block\";
  }
};
" vu-id preset-panel-id vu-id preset-panel-id)))


(defun svg-play ()
  (labels ((inner (time)
             (unless (zerop (get-val transport))
               (when (> (get-val svg-shift) (get-val svg-width)) (set-val transport 0))
               (set-val svg-shift (+ (get-val svg-shift) (* 1.067 (float (get-val svg-timescale)))))
                 (let ((next (+ time 1/60)))
                   (cm:at next #'inner next)))))
    (inner (cm:now))))
;;;(funcall my-watch)


#|

(set-val svg-piano-roll 0)
(set-val svg-scale 9.5)
(set-val svg-bar-lines 0)
(set-val svg-staff-systems 0)

(get-val svg-width)

(progn
 (set-val cursor-pos 0.2)
 (set-val cursor-pos 0.5))
;;; (ql:quickload '(clack websocket-driver alexandria cm-all))


1000% - 1094
500% - 517
100% 60 .. -60
200% 60 .. -180
300% 60 .. -300
400& 60 .. -420

(- (* 2 (+ 517 60)) 60)

60 .. -60 bei 100%

|#
