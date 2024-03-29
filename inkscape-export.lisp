;;; 
;;; inkscape-export.lisp
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

(defparameter *osc-inkscape-export-in* nil)
;;; (defparameter cl-user::*tmpsnd* nil)

#|
(defun port-available-p (portno)
  (string= ""
           (string-trim '(#\NEWLINE)
                        (with-output-to-string (out)
                          (uiop::run-program (format nil "lsof -i:~d" portno)
                                             :ignore-error-status t
                                             :output out)))))
|#

(defmacro start-inkscape-osc (&optional (osc-conn '*osc-inkscape-export-in*))
  `(if (ou:port-available-p 1337)
       (progn
         (setf ,osc-conn (incudine.osc:open :port 1337 :host "127.0.0.1" :direction :input :protocol :udp))
         (incudine:make-osc-responder ,osc-conn "/inkscape/play" ""
                                      (lambda () (load #P"/tmp/incudine-export.lisp")
;;;                           (format t "~&~S~%~%" cl-user::*tmpsnd*)
                                        )
                                      )
         (incudine:recv-start ,osc-conn)
         :inkscape-osc-rcv-started)
       (warn "port 1337 already open!")))

(defmacro stop-inkscape-osc (&optional (osc-conn '*osc-inkscape-export-in*))
  `(progn
     (incudine:remove-all-responders ,osc-conn)
     (if ,osc-conn (incudine.osc:close ,osc-conn))
     (setf ,osc-conn nil)))

 ;;; (stop-inkscape-osc)
;;; (start-inkscape-osc)

(export '(*osc-inkscape-export-in* start-inkscape-osc stop-inkscape-osc) :cm)
