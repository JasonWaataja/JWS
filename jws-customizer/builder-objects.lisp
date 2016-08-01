;;;; builder-objects.lisp - utilites for using gtk-builder

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

(defclass builder-objects-list ()
  ((object-list :initarg :object-list
		:initform nil
		:accessor object-list)))

(defmacro with-builder-objects-list ((objects-list &rest varlist) &body body)
  "Uses the given variables from varlist contained in objects-list where an
element of varlist is \(new-symbol symbol-in-object-list-alist\)"
  (let ((as-list (gensym)))
    `(let ((,as-list (object-list ,objects-list)))
       (let ,(loop for pair in varlist
		collect `(,(first pair) (cdr (assoc ',(second pair) ,as-list))))
	 ,@body))))

(define-condition builder-object-not-found (error)
  ((builder :initarg :builder
	    :reader builder)
   (object-id :initarg :object-id
	      :reader object-id)))

(defun add-builder-object (object-symbol object list)
  (with-accessors ((object-list object-list)) list
    (setf object-list (nconc object-list (list (cons object-symbol object))))))
;; (push (cons object-symbol object) (slot-value list 'object-list)))

(defun load-builder-objects (paths varlist)
  "Loads the selected objects into a builder-objects-list and returns it.
varlist should be a list with elements in the form \(symbol \"object-id\"\)"
  (if (atom paths)
      (setf paths (list paths)))
  (let ((builder (gtk-builder-new))
	(objects-list (make-instance 'builder-objects-list)))
    (dolist (path paths)
      (gtk-builder-add-from-file builder path))
    (dolist (pair varlist objects-list)
      (let ((object (gtk-builder-get-object builder (second pair))))
	(if object
	    (add-builder-object (first pair) object objects-list)
	    (restart-case (error 'builder-object-not-found
				 :builder builder
				 :object-id (second pair))
	      (skip-object () nil)))))))
