;;;; package.lisp

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

(defpackage #:jws
  (:use #:cl
	#:sb-thread
	#:gtk #:gdk #:gdk-pixbuf #:gobject
	#:glib #:gio #:pango #:cairo)
  (:export #:jws-application-main #:main))
