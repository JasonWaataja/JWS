;;;; jws.lisp - main file for JWS

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

;; Possibly the worst and least optimized queue implementation ever written.
(defclass queue ()
  ((internal-list :initform nil)))

(defun queue-is-empty (queue)
  (if (slot-value queue 'internal-list) nil t))

(defun enqueue (queue data)
  (push data (slot-value queue 'internal-list)))

(defun dequeue (queue)
  (with-slots (internal-list) queue
    (let ((cur-cons internal-list)
	  (prev-cons nil))
      (loop while (cdr cur-cons) do
	   (setf prev-cons cur-cons)
	   (setf cur-cons (cdr cur-cons)))
      (if prev-cons
	  (setf (cdr prev-cons) nil)
	  (setf internal-list nil))
      (car cur-cons))))

(defclass jws-application (gtk-application)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "JwsApplication"))

(register-object-type-implementation "JwsApplication"
				     jws-application
				     "GtkApplication"
				     nil
				     nil)

;; I'm not sure if I should be locking this too but it seems like thad would
;; break whenever the view tries to access it so I'm not sure if it's a good
;; idea.
(defvar *images-tree* (make-instance 'gtk-tree-store
				     :column-types '("gchararray" ;path
						     "gchararray" ;name
						     "gchararray" ;directory
						     "GdkPixbuf")))

(defun apply-changes ()
  (write-jws-info-to-file (get-current-jws-info)))

(defun on-apply-clicked (button)
  (declare (ignore button))
  (write-jws-info-to-file (get-current-jws-info)))

(defmacro simple-action-in-map ((name action-map) &body body)
  (let ((action (gensym))
	(action-arg (gensym))
	(parameter-arg (gensym)))
    `(let ((,action (g-simple-action-new ,name nil)))
       (g-action-map-add-action ,action-map ,action)
       (g-signal-connect ,action "activate"
			 (lambda (,action-arg ,parameter-arg)
			   (declare (ignore ,action-arg ,parameter-arg))
			   ,@body)))))

;;The global store of builder objects of type builder-objects-list
(defvar *builder-objects* (make-instance 'builder-objects-list))

(defun jws-load-builder-objects ()
  (setf *builder-objects*
	(load-builder-objects
	 ;; I'm not sure how it will find this when the app is installed.
	 "jwswindow.ui"
	 '((main-window "main_window")
	   (rotate-button "rotate_button")
	   (time-unit-box "time_unit_box")
	   (apply-button "apply_button")
	   (time-button "time_button")
	   (selection-box "selection_box")
	   (add-button "add_button")
	   (remove-button "remove_button")
	   (cancel-button "cancel_button")
	   (tree-view "tree_view")
	   (randomize-button "randomize_button")
	   (add-directory-button "add_directory_button")
	   (up-button "up_button")
	   (down-button "down_button")
	   (rotate-items-box "rotate_items_box")))))

(defvar *tree-view-selection*)

(defun get-main-window ()
  (with-builder-objects-list (*builder-objects*
			      (main-window main-window))
    main-window))

(defun set-widgets-visibility (&rest args)
  "Pass in pairs of widget, then visibility."
  (let ((cur-arg args))
    (loop while cur-arg do
	 (let ((widget (car cur-arg)))
	   (setf cur-arg (cdr cur-arg))
	   (let ((visibility (car cur-arg)))
	     (setf (gtk-widget-visible widget) visibility)))
	 (setf cur-arg (cdr cur-arg)))))

(defvar *row-side-buttons* nil)

(defun load-row-side-buttons ()
  (setf *row-side-buttons* (with-builder-objects-list
			       (*builder-objects*
				(remove-button remove-button)
				(up-button up-button)
				(down-button down-button)
				(cancel-button cancel-button))
			     (list remove-button up-button down-button
				   cancel-button))))

(defun show-selected-side-buttons (selected)
  "If selected is true, show the Remove and Cancel buttons. If it's false, then
show the cancel buttons."
  (dolist (widget *row-side-buttons*)
    (setf (gtk-widget-visible widget) selected)))

(defparameter *image-preview-height* 60)

(defun create-preview-pixbuf (path)
  (let ((original (handler-case (gdk-pixbuf-new-from-file path)
		    ;;Couldn't catch the specific error. I thought it was
		    ;;supposed to return nil when there was an error but it just
		    ;;gives a glib:g-error-condition.
		    (error () nil))))
    (when original
      ;;I think that when is redundant.
      (make-scaled-pixbuf original :height *image-preview-height*))))

(defun add-file (pathname &optional (parent-iter nil))
  ;;Note, uses lisp pathnames and not strings, probably still works either way.
  (flet ((list-directory (pathname)
	   (let ((as-wild (make-pathname :name :wild
					 :type :wild
					 :defaults pathname)))
	     (directory as-wild))))
    (let ((probe-res (probe-file pathname)))
      (if (pathname-name probe-res)
	  (progn
	    (let ((iter (gtk-tree-store-append
			 *images-tree*
			 parent-iter)))
	      (gtk-tree-store-set *images-tree* iter
				  (namestring probe-res)
				  (file-namestring probe-res)
				  "File"
				  (create-preview-pixbuf (namestring
							  probe-res)))))
	  (let ((iter (gtk-tree-store-append *images-tree* parent-iter)))
	    (gtk-tree-store-set *images-tree* iter
				(namestring probe-res)
				(car (last (pathname-directory probe-res)))
				"Directory"
				nil)
	    (dolist (child (list-directory probe-res))
	      (add-file child iter)))))))

(defun add-directory-selection ()
  (let ((dialog (gtk-file-chooser-dialog-new "Choose Directory"
					     (get-main-window)
					     :select-folder
					     "gtk-open" :accept
					     "gtk-cancel" :cancel)))
    (gtk-file-chooser-set-current-folder dialog
					 (namestring (user-homedir-pathname)))
    (gtk-file-chooser-set-select-multiple dialog t)
    (let ((response (gtk-dialog-run dialog))
	  (paths nil))
      (if (eql response :accept)
	  (setf paths (copy-seq (gtk-file-chooser-get-filenames dialog))))
      ;; Destroy dialog before adding file so it doesn't hang.
      (gtk-widget-destroy dialog)
      (when paths
	(dolist (path paths) (add-file path))))))

(defun add-file-selection ()
  (let ((dialog (gtk-file-chooser-dialog-new "Choose Image"
					     (get-main-window)
					     :open
					     "gtk-open" :accept
					     "gtk-cancel" :cancel)))
    (gtk-file-chooser-set-current-folder dialog
					 (namestring (user-homedir-pathname)))
    (gtk-file-chooser-set-select-multiple dialog t)
    (let ((filter (make-instance 'gtk-file-filter)))
      (gtk-file-filter-add-pixbuf-formats filter)
      (gtk-file-chooser-set-filter dialog filter))
    (let ((response (gtk-dialog-run dialog))
	  (paths nil))
      (if (eql response :accept)
	  (setf paths (gtk-file-chooser-get-filenames dialog)))
      ;; Destroy dialog before adding file so it doesn't hang.
      (gtk-widget-destroy dialog)
      (when paths
	(dolist (path paths) (add-file path))))))

(defvar *selected-rows* nil)

(defun on-selection-changed (selection)
  (let ((selected-list (gtk-tree-selection-get-selected-rows
			selection)))
    (if selected-list
	(let ((as-row-references (loop for path in selected-list
				    collect (gtk-tree-row-reference-new
					     *images-tree*
					     path))))
	  (setf *selected-rows* as-row-references)
	  (show-selected-side-buttons t))
	(show-selected-side-buttons nil))))

(defvar *current-jws-info* (make-instance 'jws-info))

(defun get-file-list-from-model ()
  (do ((iter (gtk-tree-model-get-iter-first *images-tree*)
	     (gtk-tree-model-iter-next *images-tree* iter))
       (file-list nil))
      ((not iter) (nreverse file-list))
    (push (gtk-tree-model-get-value *images-tree*
				    iter
				    0)
	  file-list)))

(defun message-dialog (text secondary-text &optional (type nil))
  (let ((dialog (make-instance 'gtk-message-dialog
			       :flags '(:destroy-with-parent)
			       :parent (get-main-window)
			       :message-type (or type :info)
			       :buttons :ok
			       :text text
			       :secondary-text secondary-text)))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))

(defun get-current-jws-info (&optional (display-messages t))
  (with-builder-objects-list (*builder-objects*
			      (rotate-button rotate-button)
			      (time-button time-button)
			      (time-unit-box time-unit-box)
			      (randomize-button randomize-button))
    (let ((info (make-instance 'jws-info))
	  (file-list (get-file-list-from-model)))
      (if file-list
	  (if (gtk-toggle-button-active rotate-button)
	      (let ((time-count (gtk-spin-button-get-value-as-int time-button)))
		(if (plusp time-count)
		    (progn
		      (setf (rotate-seconds info)
			    (* time-count (get-seconds-multiplier
					   (gtk-combo-box-text-get-active-text
					    time-unit-box))))
		      (setf (randomize-order info)
			    (gtk-toggle-button-active randomize-button))
		      (setf (rotate-image info) t)
		      (setf (file-list info) file-list))
		    (progn
		      (if display-messages
			  (message-dialog "Error" (format nil "Please use a ~
number greater than zero for the rotation time.") :warning))
		      (setf info nil))))
	      (progn
		(setf (rotate-image info) nil)
		(setf (file-list info) (list (first file-list)))
		(if (and (> (list-length file-list) 1) display-messages)
		    (message-dialog "Info"
				    (format nil "Rotate image is not selected, ~
using first image listed.")))))
	  (progn
	    (if display-messages
		(message-dialog "Error" (format nil "Please select at least ~
one file.")))
	    (setf info nil)))
      info)))

(defun string-split-whitespace (string)
  (let ((word-list nil)
	(on-whitespace t)
	(cur-word (make-array 0 :fill-pointer 0 :adjustable t :element-type
			      'character))
	(whitespace-chars '(#\space #\tab #\newline #\linefeed)))
    (loop for cur-char across string
       finally (unless on-whitespace (push (copy-seq cur-word) word-list))
       do
	 (let ((is-whitespace (member cur-char whitespace-chars)))
	   (if on-whitespace
	       (unless is-whitespace
		 (setf on-whitespace nil)
		 (vector-push-extend cur-char cur-word))
	       (if is-whitespace
		   (progn
		     (push (copy-seq cur-word) word-list)
		     (setf (fill-pointer cur-word) 0)
		     (setf on-whitespace t))
		   (vector-push-extend cur-char cur-word)))))

    (nreverse word-list)))


(defun get-time-info-from-seconds (seconds)
  (if (> seconds 0)
      (progn
	(let ((minute-seconds (get-seconds-multiplier "Minutes"))
	      (hour-seconds (get-seconds-multiplier "Hours")))
	  (cond ((= (mod seconds hour-seconds)0)
		 (values (/ seconds hour-seconds) "Hours"))
		((= (mod seconds minute-seconds) 0)
		 (values (/ seconds minute-seconds) "Minutes"))
		(t (values seconds "Seconds")))))
      (values 0 "Seconds")))

(defun set-current-jws-info (info)
  (when info
    (with-builder-objects-list (*builder-objects*
				(rotate-button rotate-button)
				(time-button time-button)
				(time-unit-box time-unit-box)
				(randomize-button randomize-button))
      (with-accessors ((rotate-image rotate-image)
		       (rotate-seconds rotate-seconds)
		       (randomize-order randomize-order)
		       (file-list file-list)) info
	(setf (gtk-toggle-button-active rotate-button) rotate-image)
	(multiple-value-bind (number unit)
	    (get-time-info-from-seconds rotate-seconds)
	  (setf (gtk-spin-button-value time-button) number)
	  ;;This is kind of a dumb hack.
	  (gtk-combo-box-set-active time-unit-box
				    (cond ((string-equal unit "seconds") 0)
					  ((string-equal unit "minutes") 1)
					  ((string-equal unit "hours") 2))))
	(setf (gtk-toggle-button-active randomize-button) randomize-order)
	(loop for iter = (gtk-tree-model-get-iter-first *images-tree*)
	   while iter do
	     (gtk-tree-store-remove *images-tree* iter))
	(loop for path in file-list do
	     (add-file path)))))
  info)

(defun make-scaled-pixbuf (pixbuf &key (width nil) (height nil))
  (when pixbuf
    (let ((old-width (gdk-pixbuf-width pixbuf))
	  (old-height (gdk-pixbuf-height pixbuf)))
      (let ((new-width 100)
	    (new-height 100))
	(cond ((and width height)
	       (setf new-width width new-height height))
	      ((and width (not height))
	       (setf new-width width
		     new-height (floor (* width (/ old-height old-width)))))
	      ((and height (not width))
	       (setf new-height height
		     new-width (floor (* height (/ old-width old-height))))))
	(gdk-pixbuf-scale-simple pixbuf new-width new-height :hyper)))))

(defparameter *image-window-width* 960)

(defun set-window-pixbuf (window image pixbuf)
  (gtk-image-set-from-pixbuf image pixbuf)
  (gtk-window-resize window (gdk-pixbuf-get-width pixbuf)
		     (gdk-pixbuf-get-height pixbuf)))

(defun show-dialog-for-image (path)
  (let* ((original-pixbuf (handler-case (gdk-pixbuf-new-from-file path)
			    (error () nil)))
	 (scaled-pixbuf (when original-pixbuf
					;This when is redundant.
			  (make-scaled-pixbuf original-pixbuf
					      :width *image-window-width*))))
    (if original-pixbuf
	(let ((dialog (gtk-dialog-new-with-buttons "Image"
						   (get-main-window)
						   nil
						   "gtk-close"
						   :close)))
	  (let ((image (gtk-image-new-from-pixbuf scaled-pixbuf)))
	    (gtk-container-add (gtk-dialog-get-content-area dialog) image)
	    (gtk-widget-show image)
	    (let ((original-button (gtk-button-new-with-label "Original size"))
		  (scaled-button (gtk-button-new-with-label "Scaled size")))
	      (gtk-container-add (gtk-dialog-get-action-area dialog)
				 original-button)
	      (g-signal-connect original-button "clicked"
				(lambda (button)
				  (declare (ignore button))
				  (set-window-pixbuf dialog
						     image
						     original-pixbuf)))
	      (gtk-widget-show original-button)
	      (gtk-container-add (gtk-dialog-get-action-area dialog)
				 scaled-button)
	      (g-signal-connect scaled-button "clicked"
				(lambda (button)
				  (declare (ignore button))
				  (set-window-pixbuf dialog
						     image
						     scaled-pixbuf)))
	      (gtk-widget-show scaled-button)))
	  (gtk-container-add (gtk-dialog-get-content-area dialog)
			     (gtk-label-new "Test Label"))
	  (g-signal-connect dialog "response"
			    (lambda (dialog response-id)
			      (when (eql response-id :close)
				(gtk-widget-destroy dialog))))
	  (gtk-dialog-run dialog)
	  (gtk-widget-destroy dialog))
	(message-dialog "Error"
			(format nil "Unable to display file ~a." path)))))

(defun on-remove-clicked (button)
  (declare (ignore button))
  (let ((selected-paths (gtk-tree-selection-get-selected-rows
			 *tree-view-selection*)))
    (when selected-paths
      ;; Only remove something when all paths are valid.
      (when (loop for path in selected-paths
	       do
		 (when (> (list-length (gtk-tree-path-get-indices path)) 1)
		   (message-dialog "Warning" (format nil "Can't remove ~
subfiles and subdirectories. Please only use top level items.") :warning)
		   (return nil))
	       finally
		 (return t))
	(dolist (row *selected-rows*)
	  (gtk-tree-store-remove *images-tree*
				 (gtk-tree-model-get-iter
				  *images-tree*
				  (gtk-tree-row-reference-get-path row))))))))

(defun create-sorted-row-list (row-reference-list &key (highest-first nil))
  (flet ((row-predicate (row1 row2)
	   ;; Indices.
	   (let ((ind1 (first (gtk-tree-path-get-indices
			       (gtk-tree-row-reference-get-path row1))))
		 (ind2 (first (gtk-tree-path-get-indices
			       (gtk-tree-row-reference-get-path row2)))))
	     (if highest-first
		 (>= ind1 ind2)
		 (<= ind1 ind2)))))
    (sort (copy-list row-reference-list) #'row-predicate)))

(defun on-up-clicked (button)
  (declare (ignore button))
  (when (loop for row-ref in *selected-rows*
	   do
	     (when (> (gtk-tree-path-get-depth
		       (gtk-tree-row-reference-get-path row-ref))
		      1)
	       (message-dialog "Warning" (format nil "Can only move items at ~
the top level.") :warning)
	       (return nil))
	     (when (= (first (gtk-tree-path-get-indices
			      (gtk-tree-row-reference-get-path row-ref))) 0)
	       (message-dialog "Warning" (format nil "Can't move selection. ~
Already at the front."))
	       (return nil))
	   finally
	     (return t))
    ;; (dolist (row-ref *selected-rows*)
    (dolist (row-ref (create-sorted-row-list *selected-rows*))
      (let* ((original-path (gtk-tree-row-reference-get-path row-ref))
	     (new-path (gtk-tree-path-copy original-path)))
	(unless (gtk-tree-path-prev new-path)
	  (message-dialog "Error" "Error moving selected row." :error)
	  (return))
	(let ((src-iter (gtk-tree-model-get-iter *images-tree* original-path))
	      (dest-iter (gtk-tree-model-get-iter *images-tree* new-path)))
	  (gtk-tree-store-move-before *images-tree* src-iter dest-iter))))))

(defun on-down-clicked (button)
  (declare (ignore button))
  (let ((length (gtk-tree-model-iter-n-children *images-tree* nil)))
    (when (loop for row-ref in *selected-rows*
	     for path = (gtk-tree-row-reference-get-path row-ref)
	     do
	       (when (> (gtk-tree-path-get-depth path) 1)
		 (message-dialog "Warning" (format nil "Can only move items at ~
the top level.") :warning)
		 (return nil))
	       (when (>= (first (gtk-tree-path-get-indices path))
			 (- length 1))
		 (message-dialog "Warning" (format nil "Can't move selection. ~
Item is already last.") :error)
		 (return nil))
	     finally
	       (return t))
      ;; (dolist (row-ref *selected-rows*)
      (dolist (row-ref (create-sorted-row-list *selected-rows*
					       :highest-first t))
	(let* ((original-path (gtk-tree-row-reference-get-path row-ref))
	       (new-path (gtk-tree-path-copy original-path)))
	  (unless (gtk-tree-path-next new-path)
	    (message-dialog "Error" "Error moving selected row." :error))
	  (let ((src-iter (gtk-tree-model-get-iter *images-tree*
						   original-path))
		(dest-iter (gtk-tree-model-get-iter *images-tree*
						    new-path)))
	    (gtk-tree-store-move-after *images-tree* src-iter
				       dest-iter)))))))

(defun on-rotate-clicked (button)
  (with-builder-objects-list (*builder-objects*
			      (rotate-items-box rotate-items-box))
    (setf (gtk-widget-visible rotate-items-box)
	  (gtk-toggle-button-active button))))

(defun on-add-clicked (button)
  (declare (ignore button))
  (add-file-selection))

(defun on-add-directory-clicked (button)
  (declare (ignore button))
  (add-directory-selection))

(defun open-action ()
  (let ((dialog (gtk-file-chooser-dialog-new "Open"
					     (get-main-window)
					     :open
					     "gtk-open" :accept
					     "gtk-cancel" :cancel)))
    (gtk-file-chooser-set-current-folder dialog
					 (namestring (user-homedir-pathname)))
    (gtk-file-chooser-set-select-multiple dialog nil)
    (let ((response (gtk-dialog-run dialog)))
      (when (eql response :accept)
	(let ((filename (gtk-file-chooser-get-filename dialog)))
	  (set-current-jws-info (read-jws-info-from-file filename))))
      (gtk-widget-destroy dialog))))

(defun save ()
  (write-jws-info-to-file (get-current-jws-info)))

(defun save-as ()
  (let ((dialog (gtk-file-chooser-dialog-new "Save as"
					     (get-main-window)
					     :save
					     "gtk-save" :accept
					     "gtk-cancel" :cancel)))
    (gtk-file-chooser-set-current-folder dialog
					 (namestring (user-homedir-pathname)))
    (gtk-file-chooser-set-select-multiple dialog nil)
    (let ((response (gtk-dialog-run dialog)))
      (when (eql response :accept)
	(let ((filename (gtk-file-chooser-get-filename dialog)))
	  (write-jws-info-to-file (get-current-jws-info) filename))))
    (gtk-widget-destroy dialog)))

(defun get-license ()
  (format nil
"This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or ~%(~
at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>."))

(defun show-about-dialog ()
  (gtk-show-about-dialog (get-main-window)
			 :program-name "JWS"
			 :title "About JWS"
			 :license (get-license)
			 :authors '("Jason Waataja")
			 :copyright "Copyright (C) 2016 Jason Waataja"
			 :version "Version 0.00"))

(defun jws-application-activate (application)
  (declare (ignore application))
  (within-main-loop
    (jws-load-builder-objects)
    (load-row-side-buttons)
    (with-builder-objects-list (*builder-objects*
				(main-window main-window)
				(rotate-button rotate-button)
				(time-button time-button)
				(apply-button apply-button)
				(tree-view tree-view)
				(add-button add-button)
				(remove-button remove-button)
				(add-directory-button add-directory-button)
				(up-button up-button)
				(down-button down-button)
				(rotate-items-box rotate-items-box))
      (g-signal-connect main-window "destroy" (lambda (window)
						(declare (ignore window))
						(leave-gtk-main)))
      (setf (gtk-widget-visible rotate-items-box) nil)
      (g-signal-connect rotate-button "toggled" #'on-rotate-clicked)
      (g-signal-connect apply-button "clicked" #'on-apply-clicked)
      (g-signal-connect add-button "clicked" #'on-add-clicked)
      (g-signal-connect add-directory-button "clicked"
			#'on-add-directory-clicked)
      (g-signal-connect up-button "clicked" #'on-up-clicked)
      (g-signal-connect down-button "clicked" #'on-down-clicked)
      (g-signal-connect remove-button "clicked" #'on-remove-clicked)
      (simple-action-in-map ("quit" main-window)
	(gtk-widget-destroy main-window))
      (simple-action-in-map ("save" main-window)
	(save))
      (simple-action-in-map ("save-as" main-window)
	(save-as))
      (simple-action-in-map ("open" main-window)
	(open-action))
      (simple-action-in-map ("about" main-window)
	(show-about-dialog))
      (gtk-spin-button-set-range time-button 0d0 99999d0)
      (gtk-spin-button-set-increments time-button 1d0 10d0)



      (gtk-window-present main-window)


      (gtk-tree-view-set-model tree-view *images-tree*)
      (let* ((text-renderer (gtk-cell-renderer-text-new))
	     (pixbuf-renderer (gtk-cell-renderer-pixbuf-new))
	     (name-column (gtk-tree-view-column-new-with-attributes
			   "Name"
			   text-renderer
			   "text" 1))
	     (type-column (gtk-tree-view-column-new-with-attributes
			   "Type"
			   text-renderer
			   "text" 2))
	     (image-column (gtk-tree-view-column-new-with-attributes
			    "Preview"
			    pixbuf-renderer
			    "pixbuf" 3)))
	(gtk-tree-view-append-column tree-view name-column)
	(gtk-tree-view-append-column tree-view type-column)
	(gtk-tree-view-append-column tree-view image-column))
      (setf *tree-view-selection* (gtk-tree-view-get-selection tree-view))
      (g-signal-connect *tree-view-selection* "changed" #'on-selection-changed)
      (g-signal-connect tree-view "row-activated"
			(lambda (tree-view path column)
			  (declare (ignore tree-view column))
			  (let ((iter (gtk-tree-model-get-iter *images-tree*
							       path)))
			    (let ((path (gtk-tree-model-get-value *images-tree*
								  iter
								  0))
				  (type (gtk-tree-model-get-value *images-tree*
								  iter
								  2)))
			      (when (string-equal type "File")
				(show-dialog-for-image path))))))
      ;; (setf *load-previews-thread* (make-thread #'load-previews-thread-run
      ;; 						:name "load-previews"))
      (show-selected-side-buttons nil))))
      ;; (gtk-widget-show-all main-window))))

(defun jws-application-startup (application)
  (declare (ignore application)))

(defun jws-application-open (application)
  (declare (ignore application))
  nil)

(defun jws-application-shutdown (application)
  (declare (ignore application))
  ;; (terminate-thread *load-previews-thread*))
  )

(defmethod initialize-instance :after
    ((app jws-application) &key &allow-other-keys)
    (g-signal-connect app "activate" #'jws-application-activate)
    (g-signal-connect app "startup" #'jws-application-startup)
    (g-signal-connect app "open" #'jws-application-open)
    (g-signal-connect app "shutdown" #'jws-application-shutdown))

(defun jws-application-new ()
  (g-set-application-name "JWS")
  (make-instance 'jws-application
		 :application-id "com.waataja.jws"
		 :register-session t))

(defun jws-application-main ()
  (let ((app (jws-application-new)))
    (let ((result (g-application-run app 0 (cffi-sys:null-pointer))))
      (g-object-unref (pointer app))
      result)))

(defun main (&optional argv)
  (declare (ignore argv))
  (jws-application-main))
