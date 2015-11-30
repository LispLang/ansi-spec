(in-package :cl-user)
(defpackage ansi-spec.traverse
  (:use :cl)
  (:export :traverse)
  (:documentation "Traverse a TeX document, ignoring some nodes, extracting info
  from others into an output XML file."))
(in-package :ansi-spec.traverse)

;;; Some utilities

(defun next-nth-siblings (child n)
  "Return the next nth siblings of this Plump node."
  (let ((results (list))
        (current child))
    (loop for i from 1 to n do
      (let ((sibling (plump:next-sibling current)))
        (setf current sibling)
        (push sibling results)))
    (reverse results)))

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
  ((name :reader mode-name
         :initarg :name
         :type string
         :documentation "The name that triggers the mode.")
   (callbacks :reader mode-callbacks
              :initarg :callbacks
              :initform (list)
              :type list
              :documentation "A list of functions that take a node as their sole argument."))
  (:documentation "A parser mode.")

(defparameter *modes* (make-hash-table :test #'equal)
  "A map of node names to mode objects.")

(defun get-mode (tag-name)
  "Find a node by tag-name. Return NIL if none is found."
  (gethash tag-name *modes*))

(defparameter *node-callbacks* (make-hash-table :test #'eq)
  "A map of Plump nodes to functions.")

(defun attach-callback (node callback)
  "Attach a callback to a Plump node."
  (setf (gethash node *node-callbacks*) callback))

(defun detach-callback (node)
  "Remove a callback from a Plump node."
  (remhash node *node-callbacks*))

(defmacro define-mode ((tag-name node pos &key (arity 1)) &body body)
  "Define a mode."
  (let ((tag (gensym)))
    `(let ((,tag ,tag-name))
       (setf (gethash ,tag *modes*)
             (make-instance 'mode
                            :name ,tag
                            :arity ,arity
                            :callback (lambda (,node ,pos)
                                        ,@body))))))

(defun on-node (node)
  "Dispatch a node."
  ;; If it has a tag, see if there's a corresponding mode.
  (when (plump:element-p node)
    (let ((tag (plump:tag-name node)))
      (if (get-mode tag)
          ;; Activate the mode
          (when (> (mode-arity (get-mode tag)) 0)
            (activate-mode tag))
          ;; Warn the user
          (warn "Tag ~S has no corresponding mode" tag))))
  ;; Dispatch it to the current mode
  (let ((mode (current-mode)))
    (if mode
        ;; If we have an active mode, call its callback
        (progn
          (funcall (mode-callback mode)
                   node
                   (- (mode-arity mode) (current-mode-arity)))
          ;; Lower the mode's arity
          (lower-mode-arity)
          ;; If the mode has consumed all the nodes it needs, shut it down
          (when (mode-ended-p)
            (format t "~%Deactivate: ~A" (mode-name mode))
            (deactivate-current-mode)))
        ;; If we don't, and the node is a text node, write it to the output
        ;; stream
        (when (plump:text-node-p node)
          (output (plump:text node))))))

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
