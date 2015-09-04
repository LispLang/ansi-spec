(in-package :cl-user)
(defpackage ansi-spec.traverse
  (:use :cl)
  (:export :traverse)
  (:documentation "Traverse a TeX document, ignoring some nodes, extracting info
  from others into an output XML file."))
(in-package :ansi-spec.traverse)

;;; Output stream

(defvar *stream*)

(defun output (string)
  "Write a string to the output stream."
  (write-string string *stream*))

(defun strip-text (text)
  "Remove some text nonsense from some text."
  (ppcre:regex-replace-all "/"
                           (ppcre:regex-replace-all "\\/" text "")
                           ""))

;;; Modes

(defclass mode ()
  ((arity :reader arity
          :initarg :arity
          :initform 0
          :type integer
          :documentation "The number of blocks, or bodies, the mode consumes.")
   (callback :reader callback
             :initarg :callback
             :type function
             :documentation "A function that is called on each node."))
  (:documentation "A parser mode."))

(defparameter *modes* (make-hash-table :test #'equal)
  "A map of node names to mode objects.")

(defparameter *mode-counter* (make-hash-table :test #'equal)
  "A map of node names to the number of node they have yet to consume.")

(defparameter *active-nodes* (list)
  "A list of node tags.")

(defun get-mode (tag-name)
  "Find a node by tag-name. Return NIL if none is found."
  (gethash tag-name *modes*))

(defun activate-mode (tag-name)
  "Activate a node."
  (push tag-name *active-nodes*))

(defun deactivate-last-mode ()
  "Turn off the current active node."
  (pop *active-nodes*))

(defun current-mode ()
  "Return the current active mode object, or NIL."
  (get-mode (first *active-nodes*)))

(defmacro define-mode ((tag-name node &key (arity 1)) &body body)
  "Define a mode."
  `(setf (gethash ,tag-name *modes*)
         (make-instance 'mode
                        :arity ,arity
                        :callback (lambda (,node)
                                    ,@body))))

(defun on-node (node)
  "Dispatch a node."
  ;; If it has a tag, see if there's a corresponding mode.
  (when (plump:element-p node)
    (let ((tag (plump:tag-name node)))
      (if (get-mode node)
          ;; Activate the mode
          (activate-mode tag)
          ;; Warn the user
          (warn "Tag ~S has no corresponding mode" tag))))
  ;; Dispatch it to the current mode
  (let ((mode (current-mode)))
    (if mode
        ;; If we have an active mode, call its callback
        (funcall (callback mode) node)
        ;; If we don't, and the node is a text node, write it to the output
        ;; stream
        (when (plump:text-node-p node)
          (output (plump:text node))))))

;;; A breadth-first version of Plump's traverse

(defgeneric traverse-tree (node)
  (:documentation "Traverse a Plump document in breadth-first order."))

;;; Interface

(defun traverse (pathname)
  "Traverse the document in pathname."
  (format t "~&Traversing '~A.tex'" (pathname-name pathname))
  (ansi-spec.file:with-output-file (*stream*)
    (plump:traverse (plump-tex:parse
                     (ansi-spec.preprocess:preprocess
                      (uiop:read-file-string pathname)))
                    #'(lambda (node)
                        (on-node node)))))
