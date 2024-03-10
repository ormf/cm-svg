;;; 
;;; display-automation.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022-24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-user)

(defpackage #:cm.svgd
  (:use #:cl)
  (:export #:shift #:cursor-pos #:width #:scale #:seq #:inverse #:timescale
           #:piano-roll #:staff-systems #:bar-lines #:idx #:data
           #:transport #:auto-return #:play-watch #:data-watch #:timescale-watch))

(in-package :cm.svgd)

(defparameter cursor-pos nil)
(defparameter shift nil)
(defparameter width nil)
(defparameter scale nil)
(defparameter seq nil)
(defparameter inverse nil)
(defparameter timescale nil)
(defparameter piano-roll nil)
(defparameter staff-systems nil)
(defparameter bar-lines nil)
(defparameter idx nil)
(defparameter data nil)
(defparameter transport nil)
(defparameter auto-return (cl-refs:make-ref 0))
(defparameter play-watch nil)
(defparameter timescale-watch nil)
(defparameter data-watch nil)


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

(defun get-timescale (&rest tempo)
  "calc svg timescale from tempo"
  (/ 15/4 (apply #'* tempo)))

(defun object-end (obj)
  (typecase obj
    (poolevt
     (let* ((end (sv obj :end))
            (buffer (of-incudine-dsps:lsample-buffer (sv obj :lsample)))
            (bufdur (float (/ (incudine::buffer-frames buffer)
                              (incudine::buffer-sample-rate buffer))
                           1.0)))
       (+ (object-time obj) (* (sv obj :stretch) (if (zerop end) bufdur (min end bufdur))))))
    (t (+ (object-time obj) (sv obj :duration)))))

(defun trim-start (obj curr-pos obj-end)
  (typecase obj
    (poolevt
     (setf (sv obj :start) (/ (- curr-pos (object-time obj)) (sv obj :stretch)))
     (sv* obj :stretch (get-val cm.svgd:timescale)))
    (t (setf (sv obj :duration)
             (* (get-val cm.svgd:timescale) (- obj-end curr-pos)))))
;;  (break "~a" obj)
  (values))

(defun seq-play (obj)
  (let* ((evts (subobjects obj))
;;;         (offs (get-first-in-region evts region timescale))
         )
    (if evts 
        (progn
          (let ((curr-pos (get-val cm.svgd:shift)))
            (dolist (obj evts)
              (let ((obj-end (object-end obj)))
                (when (>= obj-end curr-pos)
                  (let ((obj (copy-object obj)))
                    (if (< (object-time obj) curr-pos)
                        (progn
                          (trim-start obj curr-pos obj-end)
                          (setf (sv obj :time) 0))
                        (progn
                          (setf (sv obj :time)
                                (float (* (get-val cm.svgd:timescale)
                                          (- (sv obj :time) curr-pos))))
                          (typecase obj
                            (poolevt (sv* obj :stretch (get-val cm.svgd:timescale)))
                            (t (sv* obj :duration (get-val cm.svgd:timescale))))))
;;;                    (format t "~a~%" obj)
                    (sprout obj))
                  ))
              ))
          ;;          (browser-play (* offs 6.041) :tscale (/ 1/8 6.041))
          ))))

#|
(sv (new poolevt) :lsample)




#i(poolevt time 90.7 lsample #S(of-incudine-dsps:lsample
                                :filename "/home/orm/work/kompositionen/letzte-worte/snd/samples/fl-s01-line10.wav"
                                :buffer #<incudine:buffer :FRAMES 469921 :CHANNELS 2 :SR 48000.0>
                                :play-fn nil
                                :keynum 62.599998474121094d0
                                :loopstart 0.0d0
                                :amp 0.0d0
                                :loopend 0.0d0
                                :id nil) keynum 62.6 amp 0.5 dy -4.6 start 0 end 0 stretch 4.9999890481924085d0 wwidth 123 attack 0 release 0.01 pan 0.5 snd-id 6 adjust-stretch nil out1 0 out2 1).
[Condition of type sb-pcl::missing-slot]
|#


(defmacro sv- (obj slot val &body more) (svaux obj '- slot val more))

(progn
  (clear-bindings)
  (when cm.svgd:play-watch (funcall cm.svgd:play-watch))
  (when cm.svgd:data-watch (funcall cm.svgd:data-watch))
  (setf cm.svgd:cursor-pos (make-ref 0.5))
  (setf cm.svgd:inverse (make-ref 0))
  (setf cm.svgd:shift (make-ref 0))
  (setf cm.svgd:seq (make-ref nil))
  (setf cm.svgd:width (make-ref 0))
  (setf cm.svgd:scale (make-ref 9.5))
  (setf cm.svgd:timescale (make-ref (get-timescale 1/4 96)))
  (setf cm.svgd:piano-roll (make-ref 1))
  (setf cm.svgd:staff-systems (make-ref 1))
  (setf cm.svgd:bar-lines (make-ref 1))
  (setf cm.svgd:idx (make-ref 0))
  (setf cm.svgd:transport (make-ref 0))
  (setf cm.svgd:data (make-ref ""))
  #|
  (setf cm.svgd:play-watch (watch (let ((last-pos 0))
                            (lambda () (if (zerop (get-val cm.svgd:transport))
                                      (let (cl-refs::*curr-ref*)
;;                                        (format t "stopping~%")
                                        (incudine:node-free-all)
                                        (incudine:flush-pending)
                                        (unless (zerop (get-val cm.svgd:auto-return))
                                          (set-val cm.svgd:shift last-pos )))
                                      (let (cl-refs::*curr-ref*)
;;                                        (format t "relocating~%")
                                        (setf last-pos (get-val cm.svgd:shift))
                                        (seq-play (get-val cm.svgd:seq))
                                        (svg-play)

))))))
  (setf cm.svgd:data-watch (watch (lambda ()
;;;                            (format t "reset-seq")
                                    (set-val cm.svgd:seq
                                             (let ((seq
                                                     (cm:import-events
                                                      (namestring
                                                       (merge-pathnames
                                                        (format nil "www/~A" (get-val cm.svgd:data))
                                                        (asdf:system-source-directory :clog-dsp-widgets)))
                                                      :x-scale 1)))
                                               (when seq
                                                 (setf (container-subobjects seq) (sort (subobjects seq) #'< :key #'object-time)))
  seq)))))
  |#
  nil)

;;; (find-object "hdbg04q-sfz-seq")



;;; (get-val cm.svgd:seq)
;;; (set-val svg-timescale 1)



;;, (set-val cm.svgd:data "hdbg04b-sfz.svg")
(set-val cm.svgd:data "hdbg04q-sfz.svg")
;;; (set-val cm.svgd:scale 9.5)
;;; (get-val cm.svgd:seq)
;;; (get-val cm.svgd:width)

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

(defun svg-display (body)
  "On-new-window handler."
  (let (transport-toggle)
    (setf (clog:title (clog:html-document body)) "SVG Test")
    (create-o-svg
     body (bind-refs-to-attrs cm.svgd:width "width"
                              cm.svgd:cursor-pos "cursor-pos"
                              cm.svgd:shift "shift-x"
                              cm.svgd:data "data"
                              cm.svgd:scale "scale"
                              cm.svgd:piano-roll "piano-roll"
                              cm.svgd:staff-systems "staff-systems"
                              cm.svgd:bar-lines "bar-lines"
                              cm.svgd:inverse "inverse"))
    (create-o-slider body (bind-refs-to-attrs cm.svgd:shift "value" cm.svgd:width "max")
                     :min 0 :max 200 :direction :right
                     :css `(:display "inline-block" :height "1em" :width "100%"))
    (setf transport-toggle
          (create-o-toggle body (bind-refs-to-attrs cm.svgd:transport "value")
                           :label '("play" "stop") :background '("transparent" "#8f8")
                           :css `(:display "inline-block" :height "1.2em" :width "3em")))
    (create-o-toggle body (bind-refs-to-attrs cm.svgd:auto-return "value")
                     :label '("rtn") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (create-o-toggle body (bind-refs-to-attrs cm.svgd:piano-roll "value")
                     :label '("pno") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (create-o-toggle body (bind-refs-to-attrs cm.svgd:staff-systems "value")
                     :label '("stf") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (create-o-toggle body (bind-refs-to-attrs cm.svgd:bar-lines "value")
                     :label '("bar") :css `(:display "inline-block" :height "1.2em" :width "3em"))
    (set-keyboard-shortcuts body transport-toggle)
    ))

(clog:set-on-new-window 'svg-display :path "/svg-display" :boot-file "/start.html")

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
  (clog:set-on-new-window  'svg-display :path "/svg-display" :boot-file "/start.html")
  (clog:open-browser :url "http://127.0.0.1:8080/svg-display"))

;;; (start) should start a webserver with some gui widgets that are
;;; connected

;;; (start-svg-display)

;; (set-val cursor-pos 0.5)
;; (set-val svg-width 8000)

;; (set-val svg-scale 9.5)

;;; (set-val svg-timescale 0.125) 


(defun svg-play ()
  (labels ((inner (time)
             (unless (zerop (get-val cm.svgd:transport))
               (when (> (get-val cm.svgd:shift) (+ 2 (get-val cm.svgd:width))) (set-val cm.svgd:transport 0))
               (set-val cm.svgd:shift (+ (get-val cm.svgd:shift)
                                         (* 1.067 (float (/ 1/64 (get-val cm.svgd:timescale))))))
                 (let ((next (+ time 1/60)))
                   (cm:at next #'inner next)))))
    (inner (cm:now))))

;;;(funcall my-watch)


#|

(set-val cm.svgd:piano-roll 0)
(set-val cm.svgd:scale 9.5)
(set-val cm.svgd:bar-lines 0)
(set-val cm.svgd:staff-systems 0)

(set-val cm.svgd:timescale 0.125)
(set-val cm.svgd:timescale 0.25)
(get-val cm.svgd:width)

(progn
 (set-val cm.svgd:cursor-pos 0.2)
 (set-val cm.svgd:cursor-pos 0.5))
;;; (ql:quickload '(clack websocket-driver alexandria cm-all))

|#
