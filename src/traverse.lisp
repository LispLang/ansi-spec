(in-package :cl-user)
(defpackage ansi-spec.traverse
  (:use :cl)
  (:export :traverse)
  (:documentation "Traverse a TeX document, ignoring some nodes, extracting info
  from others into an output XML file."))
(in-package :ansi-spec.traverse)

;;; Some utilities

(defun next-n-siblings (child n)
  "Return the next n siblings of this Plump node."
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
              :type list
              :documentation "A list of functions that take a node as their sole
              argument.")
   (final-callback :reader mode-final-callback
                   :type function
                   :documentation "A function that takes a node as its argument
                   and is called after all callbacks are processed."))
  (:documentation "A parser mode."))

(defun mode-arity (mode)
  "Return the arity of the mode."
  (length (mode-callbacks mode)))

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

(defun try-callback (node)
  "If this mode has an associated callback, call it, and detach the callback."
  (multiple-value-bind (callback found)
      (gethash node *node-callbacks*)
    (when found
      (funcall callback node)
      (detach-callback node))))

(defmacro define-mode ((tag-name) &key callbacks)
  "Define a mode."
  (let ((tag (gensym)))
    `(let ((,tag ,tag-name))
       (setf (gethash ,tag *modes*)
             (make-instance 'mode
                            :name ,tag
                            :callbacks
                            (list ,@callbacks))))))

(defun on-node (node)
  "Dispatch a node."
  ;; If it has a tag, see if there's a corresponding mode.
  (when (plump:element-p node)
    (let ((tag (plump:tag-name node)))
      (if (get-mode tag)
          ;; Trigger the mode
          (let* ((mode (get-mode tag))
                 (arity (mode-arity mode)))
            (when (> arity 0)
              ;; Find the next `arity-1` siblings, and attach all but the first
              ;; callbacks to them. The first callback is called immediately
              ;; with the current node
              (funcall (first (mode-callbacks mode)) node)
              (let ((siblings (next-n-siblings node (1- arity))))
                (loop for i from 0 to (1- (length siblings)) do
                  (attach-callback (nth i siblings)
                                   (nth i (rest (mode-callbacks mode))))))))
          ;; Warn the user
          (warn "Tag ~S has no corresponding mode" tag))))
  ;; Try to call this node's callback, if any
  (try-callback node)
  ;; If the node is a text node, write it to the output stream
  (when (plump:text-node-p node)
    (output (plump:text node))))

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
