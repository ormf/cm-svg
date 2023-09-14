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

(in-package :cl-user)

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
