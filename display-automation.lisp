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

(export 'sfz 'cm)

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

(defun region-play (name &key (absolute nil) (timescale 1) (region '(0 nil)) (durfac 1) (pstretchfn (lambda (x) (declare (ignore x)) 1)))
  (let* ((obj (if absolute
                  (import-events name :x-scale timescale)
                  (import-events (format nil "/home/orm/work/kompositionen/heidelberg/grafik/~a.svg" name) :x-scale timescale)))
         (evts (if obj (sort (subobjects obj) #'< :key #'object-time)))
;;;         (offs (get-first-in-region evts region timescale))
         )
    (if evts
        (progn
;;;          (free-all-voices)
          (sprout (mapcar (lambda (evt) (let ((keynum (sv evt :keynum)))
                                     (sv* evt :duration durfac)
                                     (if (typep evt 'sfz) (sv+ evt :amplitude 12)
                                         (sv* evt :amplitude 2))
                                     (setf (sv evt :keynum)
                                           (+ 36 (* (- keynum 36)
                                                    (funcall pstretchfn (1+ (* 1/16 (/ (object-time evt) timescale)))))))
                                     evt))
                          (apply #'trim-region evts
                                 (mapcar (lambda (x) (if x (beat->time (* 16 (- x 1)) :factor timescale))) region))))
;;          (browser-play (* offs 6.041) :tscale (/ 1/8 6.041))
          )
        (error "obj ~a not found!" (format nil "~a-seq" name)))))

(in-package :clog-dsp-widgets)

(progn
  (defparameter cursor-pos nil)
  (defparameter svg-shift nil)
  (defparameter svg-width nil)
  (defparameter svg-scale nil)
  (defparameter svg-piano-roll nil)
  (defparameter svg-staff-systems nil)
  (defparameter svg-bar-lines nil)
  (defparameter idx nil)
  (defparameter data nil)
  (defparameter transport nil)
  (defparameter auto-return (make-ref 0)))

(progn
  (clear-bindings)
  (setf cursor-pos (make-ref 0.5))
  (setf svg-shift (make-ref 0))
  (setf svg-width (make-ref 0))
  (setf svg-scale (make-ref 20))
  (setf svg-piano-roll (make-ref 1))
  (setf svg-staff-systems (make-ref 1))
  (setf svg-bar-lines (make-ref 1))
  (setf idx (make-ref 0))
  (setf transport (make-ref 0))
  (setf data (make-ref "hdbg04b-sfz.svg"))
  nil)

(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "SVG Test")
  (create-o-svg
   body (bind-refs-to-attrs svg-width "width"
                            cursor-pos "cursor-pos"
                            svg-shift "shift-x"
                            data "data"
                            svg-scale "scale"
                            svg-piano-roll "piano-roll"
                            svg-staff-systems "staff-systems"
                            svg-bar-lines "bar-lines")
   :svg "/html-display.svg")
;;;  (create-o-radio body (bind-refs-to-attrs idx "value") :css '(:width "6em") :labels (list (loop for idx from 1 to 6 collect idx)) :num 6)
  (create-o-slider body (bind-refs-to-attrs svg-shift "value" svg-width "max")
                   :min 0 :max 200 :direction :right
                   :css `(:display "inline-block" :height "1em" :width "100%"))
  (create-o-toggle body (bind-refs-to-attrs transport "value")
                   :label '("play" "stop") :background '("transparent" "#8f8")
                   :css `(:display "inline-block" :height "1.2em" :width "3em"))
  (create-o-toggle body (bind-refs-to-attrs auto-return "value")
                   :label '("rtn") :css `(:display "inline-block" :height "1.2em" :width "3em"))
  (create-o-toggle body (bind-refs-to-attrs svg-piano-roll "value")
                   :label '("pno") :css `(:display "inline-block" :height "1.2em" :width "3em"))
  (create-o-toggle body (bind-refs-to-attrs svg-staff-systems "value")
                   :label '("stf") :css `(:display "inline-block" :height "1.2em" :width "3em"))
  (create-o-toggle body (bind-refs-to-attrs svg-bar-lines "value")
                   :label '("bar") :css `(:display "inline-block" :height "1.2em" :width "3em")))

(set-val svg-width 300)

(defun on-new-window (body)
  (new-window body))

;; Initialize the CLOG system with a boot file which contains the
;; static js files. For customized uses copy the "www" subdirectory of
;; the repository to your local project and adjust :static-root
;; accordingly
(defun start ()
  (clear-bindings) ;;; start from scratch
  (initialize #'on-new-window
              :port 8080
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-dsp-widgets))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

;;; (start) should start a webserver with some gui widgets that are
;;; connected

(start)

;; (set-val cursor-pos 0.5)
;; (set-val svg-width 8000)

;; (set-val svg-scale 20)

(defun play-svg ()
  (labels ((inner (time)
             (unless (zerop (get-val transport))
                 (set-val svg-shift (+ (get-val svg-shift) 0.1))
                 (let ((next (+ time 1/60)))
                   (cm:at next #'inner next)))))
    (inner (cm:now))))
;;;(funcall my-watch)

(defparameter my-watch (watch (let ((last-pos 0))
                                (lambda () (if (zerop (get-val transport))
                                          (let (cl-refs::*curr-ref*)
                                            (format t "stopping~%")
                                            (unless (zerop (get-val auto-return))
                                              (set-val svg-shift last-pos )))
                                          (progn
                                            (format t "relocating~%")
                                            (setf last-pos (let (cl-refs::*curr-ref*)
                                                             (get-val svg-shift)))
                                            (play-svg)))))))

#|

(set-val svg-piano-roll 0)
(set-val svg-bar-lines 0)
(set-val svg-staff-systems 0)

(progn
 (set-val cursor-pos 0.2)
 (set-val cursor-pos 0.5))
;;; (ql:quickload '(clack websocket-driver alexandria cm-all))

;; make a hash table to map connections to nicknames
(defparameter *connections* (make-hash-table))

;; and assign a random nickname to a user upon connection
(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-room (connection message)
  (let ((message (format nil "~a: ~a"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defparameter *ws* nil)

(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) (broadcast-to-room ws msg)))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws)))) ; send the handshake

;; keep the handler around so that you can stop your server later on

(defparameter *chat-handler* nil)

(if (ou:port-available-p 13245)
    (setf *chat-handler* (clack:clackup #'chat-server :port 13245))
    (warn "port 13245 already in use!"))

;;; (clack:stop *chat-handler*)

(defparameter *html* nil)

(defun broadcast-message (msg)
  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con msg)))

|#

#|
(defparameter *client-handler* nil)

(defun client-server (env)
    (declare (ignore env))
    `(200 (:content-type "text/html")
          (,*html*)))



(defun scroll (time xoffs)
  (unless (< xoffs -1920)
    (let ((next (+ time (* 100 (/ incudine::*sample-rate* 1000)))))
      (broadcast-message (format nil "~apx,0px" xoffs))    
      (incudine:at next #'scroll next (1- xoffs)))))

;;; (broadcast-message (format nil "~apx,0px" 500))

(defparameter *from-pd* (fudi:open :port 3010))
(defparameter *fudi-responder* nil)

(scratch::recv-start *from-pd*)

(setf *fudi-responder*
      (incudine::make-fudi-responder
       *from-pd*
       (lambda (msg)
         (broadcast-message (format nil "~{~apx,~apx~}" msg)))))

(scroll (incudine:now) 0)
(* 49.62017 (/ 496994 497777)) 49.542118

(broadcast-message "-10px,00px")

(loop for xoffs from 0 downto -300)



(progn
  (setf *html*
        "<!doctype html>

<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <title>LISP-CHAT</title>
</head>
<body>
    <img src=\"./test.svg\"> </img>

</body>
</html>
")
  (when *client-handler* (clack:stop *client-handler*))
  (setf *client-handler* (clack:clackup #'client-server :port 8080
                                                        :document-root (truename "/tmp/"))))

;;; (clack::find-handler :hunchentoot)

;;; (clack.handler.hunchentoot::run)

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

;;; (ql:where-is-system "hunchentoot")

(defvar *acceptor* (make-instance 'easy-acceptor
        :port 4242
        :document-root (truename "work/kompositionen/heidelberg/grafik/")))



(progn
  (setf *html*
      "<style>
:root{
  --height: 50px;
}

.cursor{
  position: absolute;
  top:0;
  left:50vw;
  height: 100vh;
  width: 2px;
  background-color: red;
}
#svg{
  position: absolute;
#  top:calc(50vh - var(--height)/2);
  top:0;
  height: 50vh;
  left:50vw;
  transition: 1ms linear;
}
</style>

<object id=\"svg\" data=\"https://upload.wikimedia.org/wikipedia/commons/6/62/Music_notation.svg\" type=\"image/svg+xml\"></object>
<div class=\"cursor\"></div>

<script>
let svg = document.getElementById(\"svg\") ;
let socket = new WebSocket(\"ws://localhost:12345\");

socket.onopen = function(e) {
  console.log(\"[open] Connection established\");
  console.log(\"Sending to server\");
  socket.send(\"Heartbeat\");
};

socket.onmessage = function(event) {
  console.log(`[message] Data received from server: ${event.data}`);
  // data should be string: \"x,y\" e.g. \"-100px,-50px\"
  svg.style.transform = `translate(${event.data})`;
};

socket.onclose = function(event) {
  if (event.wasClean) {
    console.log(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
  }
  else {
    // e.g. server process killed or network down // event.code is usually 1006 in this case
    console.log('[close] Connection died');
  }
};

socket.onerror = function(error) {
  console.log(`[error] ${error.message}`);
};
</script>
")
  (when *client-handler* (clack:stop *client-handler*))
  (setf *client-handler* (clack:clackup #'client-server :port 8080
                                                        :document-root (truename "/tmp/"))))


(progn
  (setf *html*
      "<style>
:root{
  --height: 50px;
}

.cursor{
  position: absolute;
  top:0;
  left:50vw;
  height: 100vh;
  width: 2px;
  background-color: red;
}
#svg{
  position: absolute;
#  top:calc(50vh - var(--height)/2);
  top:0;
  height: 100vh;
  left:50vw;
  transition: 1ms linear;
}
</style>

<object id=\"svg\" data=\"http://localhost/hdbg04l-sfz.svg\" type=\"image/svg+xml\"></object>
<div class=\"cursor\"></div>

<script>
let svg = document.getElementById(\"svg\") ;
let socket = new WebSocket(\"ws://localhost:12345\");

socket.onopen = function(e) {
  console.log(\"[open] Connection established\");
  console.log(\"Sending to server\");
  socket.send(\"Heartbeat\");
};

socket.onmessage = function(event) {
  console.log(`[message] Data received from server: ${event.data}`);
  // data should be string: \"x,y\" e.g. \"-100px,-50px\"
  svg.style.transform = `translate(${event.data})`;
};

socket.onclose = function(event) {
  if (event.wasClean) {
    console.log(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
  }
  else {
    // e.g. server process killed or network down // event.code is usually 1006 in this case
    console.log('[close] Connection died');
  }
};

socket.onerror = function(error) {
  console.log(`[error] ${error.message}`);
};
</script>
")
  (when *client-handler* (clack:stop *client-handler*))
  (setf *client-handler* (clack:clackup #'client-server :port 8080
                                                        :document-root (truename "/tmp/"))))


|#
