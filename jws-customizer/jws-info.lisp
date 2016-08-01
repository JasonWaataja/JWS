;;; jws-info.lisp - code for handling what to read and write to files

;; Copyright (C) 2016 Jason Waataja

;; This file is part of JWS.

;; JWS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; JWS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with JWS.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:jws)

(defvar *config-locations* '("~/.jws"
			     "~/.config/jws/config"))

(defvar *selected-location* (first *config-locations*))

(defun make-seconds-multipliers ()
  (let ((table (make-hash-table :test #'equalp)))
    (setf (gethash "seconds" table) 1)
    (setf (gethash "minutes" table) 60)
    (setf (gethash "hours" table) (* (gethash "minutes" table) 60))
    table))

(defparameter *seconds-multipliers* (make-seconds-multipliers))

(define-condition seconds-multiplier-not-found (error)
  ((string-value :initarg :string-value
		 :accessor string-value)))

(defun get-seconds-multiplier (as-string)
  (let ((multiplier 1))
    (multiple-value-bind (value found) (gethash as-string *seconds-multipliers*)
      (setf multiplier value)
      (unless found
	(restart-case (error 'seconds-multiplier-not-found
			     :string-value as-string)
	  (use-one () (setf multiplier 1))
	  (try-again () (setf multiplier (get-seconds-multiplier as-string)))))
      multiplier)))

(defclass jws-info ()
  ((file-list :initarg :file-list :initform nil :accessor file-list)
   (rotate-image :initarg :rotate-image :initform nil :accessor rotate-image)
   (rotate-seconds :initarg :rotate-seconds :initform 60 :accessor
		   rotate-seconds)
   (randomize-order :initarg :randomize-order :initform t :accessor
		    randomize-order)))


(defun write-jws-info-to-file (info &optional (path (first *config-locations*)))
  (when info
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede)
      (if (rotate-image info)
	  (progn
	    (write-line "rotate-image" stream)
	    (format t "~d~%" (rotate-seconds info))
	    (if (randomize-order info)
		(write-line "randomize-order" stream)
		(write-line "in-order" stream))
	    (format stream "time ~d~%" (rotate-seconds info)))
	  (write-line "single-image" stream))

      (format stream "~%")
      (write-line "files" stream)
      (dolist (path (file-list info))
	(write-line path stream)))))


(defun read-jws-info-from-file (&optional (path (first *config-locations*)))
  (let ((info (make-instance 'jws-info)))
    (with-accessors ((file-list file-list)
		     (rotate-image rotate-image)
		     (rotate-seconds rotate-seconds)
		     (randomize-order randomize-order))
	info
      (with-open-file (stream path
			      :direction :input
			      :if-does-not-exist nil)
	(when stream
	  (loop for line = (read-line stream nil)
	     while (and stream (string-not-equal line "files"))
	     do
	       (let* ((words (string-split-whitespace line))
		      (first-word (first words)))
		 (cond ((string-equal first-word "rotate-image")
			(setf rotate-image t))
		       ((string-equal first-word "single-image")
			(setf rotate-image nil))
		       ((string-equal first-word "randomize-order")
			(setf randomize-order t))
		       ((string-equal first-word "in-order")
			(setf randomize-order nil))
		       ((string-equal first-word "time")
			(setf rotate-seconds
			      (max (or (parse-integer (second words)
						      :junk-allowed t)
				       0)
				   60)))))
	     finally
	       (when (string-equal line "files")
		 (loop for line = (read-line stream nil)
		    while line do
		      (if (> (length line) 0)
			  (push line file-list)))
		 (setf file-list (nreverse file-list)))))))
    info))
