;;;; jws.asd

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


(asdf:defsystem #:jws
  :description "Progam that customizes wallpaper settings."
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "GNU General Public License v3.0"
  :depends-on (#:cl-cffi-gtk-glib
	       #:cl-cffi-gtk-gobject
	       #:cl-cffi-gtk-gdk
	       #:cl-cffi-gtk-gdk-pixbuf
	       #:cl-cffi-gtk-gio
	       #:cl-cffi-gtk-pango
	       #:cl-cffi-gtk-cairo
	       #:cl-cffi-gtk)
  ;; :serial t
  ;; :components ((:file "package")
  ;;              (:file "jws")))
  :components ((:file "package")
	       (:file "builder-objects" :depends-on ("package"))
	       (:file "jws-info" :depends-on ("package"))
	       (:file "jws" :depends-on ("package"
					 "jws-info"
					 "builder-objects"))))
