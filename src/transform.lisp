(in-package :cl-ansi-spec)

(defun strip (string)
  "Strip whitespace from the ends of a string."
  (string-trim '(#\Newline #\Space #\Tab) string))

(defparameter *transform-table* (make-hash-table :test #'equal))

(defmacro define-transform (tag-or-tag-list (attributes children) &rest body)
  (if (atom tag-or-tag-list)
      `(setf (gethash ,tag-or-tag-list *transform-table*)
             (lambda (,attributes ,children)
               ,@body))
      `(progn
         ,@(loop for tag in tag-or-tag-list collecting
             `(define-transform ,tag ,attributes ,children ,@body)))))

(defgeneric transform (object)
  (:documentation "Take a Plump node from the TeX source and manipulate it."))

(defmethod transform ((text-node plump:text-node))
  (strip (plump:text text-node)))

(defmethod transform ((vector vector))
  (loop for elem across vector collecting
    (transform elem)))

(defmethod transform ((root plump:root))
  (transform (plump:children root)))

(defmethod transform ((element plump:element))
  (let ((name (plump:tag-name element))
        (attributes (plump:attributes element))
        (children (plump:children element)))
    (aif (gethash name *transform-table*)
         (funcall it attributes (transform children))
         (format t "Could not find transform for tag '~A'~%" name))))

(defun attr (attrs name)
  (let ((attribute (gethash name attrs)))
    (when attribute
      (when (char= (elt attribute 0) #\')
        (setf attribute (subseq attribute 1)))
      (when (char= (elt attribute (1- (length attribute)))
                   #\')
        (setf attribute (subseq attribute 0 (1- (length attribute)))))
      attribute)))

;;; Sections

(define-transform "chapter" (a children)
  (list :chapter
        (list :index (attr a "index")
              :title (attr a "title")
              :chap-id (attr a "chap-id")
              :ref-title (attr a "ref-title"))
        children))

(defmacro define-section-transform (name keyword)
  `(define-transform ,name (a children)
     (list ,keyword
           (list :title (attr a "title")
                 :ref (attr a "ref"))
           children)))

(define-section-transform "section" :section)
(define-section-transform "subsection" :subsection)
(define-section-transform "subsubsection" :subsubsection)
(define-section-transform "subsubsubsection" :subsubsubsection)
(define-section-transform "subsubsubsubsection" :subsubsubsubsection)

;;; Issues

(define-transform "issue" (a children)
  `(:issue (:name ,(attr a "name"))
           ,@children))

;;; References

(defmacro define-ref-transform (name)
  `(define-transform ,name (a children)
     (declare (ignore a))
     (list :clref (list :type ,(intern (string-upcase name) :keyword))
           (first children))))

(define-ref-transform "kwd")
(define-ref-transform "kwdref")
(define-ref-transform "packref")
(define-ref-transform "loopref")

(define-ref-transform "keyref")

(define-ref-transform "typeref")
(define-ref-transform "misc")
(define-ref-transform "miscref")
(define-ref-transform "declref")
(define-ref-transform "funref")
(define-ref-transform "macref")
(define-ref-transform "specref")
(define-ref-transform "conref")
(define-ref-transform "varref")

(define-ref-transform "secref")
(define-ref-transform "chapref")
(define-ref-transform "figref")

(define-transform "bogusterm" (a children)
  (declare (ignore a))
  (first children))

(define-transform "newterm" (a children)
  (declare (ignore a))
  `(:newterm ,(first children)))

(define-transform "term" (a children)
  (declare (ignore a))
  (list :term (first children)))

(define-transform "param" (a children)
  (declare (ignore a))
  (list :param (first children)))

(define-transform "logidx" (a children)
  `(:logidx (:type ,(attr a "type")) ,(first children)))

;;; Defuns



;;; Markup

(define-transform "b" (a children)
  (declare (ignore a))
  (list :bold (first children)))

(define-transform "i" (a children)
  (declare (ignore a))
  (list :italic (first children)))

(define-transform "j" (a children)
  (declare (ignore a))
  (list :italic (first children)))

(define-transform "f" (a children)
  (declare (ignore a))
  (list :teletype (first children)))

(define-transform "rm" (a children)
  (declare (ignore a))
  (list :roman (first children)))

(define-transform "it" (a children)
  (declare (ignore a))
  (list :italic (first children)))

(define-transform "tt" (a children)
  (declare (ignore a))
  (list :teletype (first children)))

(define-transform "ital" (a children)
  (declare (ignore a))
  (list :italic (first children)))

(define-transform "bold" (a children)
  (declare (ignore a))
  (list :bold (first children)))

(define-transform "doublequotes" (a children)
  (declare (ignore a))
  (list :double-quotes (first children)))

(define-transform "ang" (a children)
  (declare (ignore a))
  `(:angle-brackets ,(first children)))

(define-transform "flr" (a children)
  (declare (ignore a))
  `(:floor ,(first children)))

(define-transform "code" (a children)
  (declare (ignore a))
  (list :code (first children)))

(define-transform "sub" (a children)
  (declare (ignore a))
  (list :sub (first children)))

(define-transform "sup" (a children)
  (declare (ignore a))
  (list :sup (first children)))

(define-transform "underlined" (a children)
  (declare (ignore a))
  `(:underlined ,(first children)))

(define-transform "metavar" (a children)
  (declare (ignore a))
  `(:metavar ,(first children)))

(define-transform "metaparam" (a children)
  (declare (ignore a))
  `(:metaparam ,(first children)))

;;; Lists

(define-transform "list" (a children)
  (declare (ignore a))
  `(:list ,@children))

(define-transform "item" (a children)
  (declare (ignore a))
  `(:list-item ,@children))

(define-transform "itemitem" (a children)
  (declare (ignore a))
  `(:list-item ,@children))

;;; Tables

(define-transform "table" (a children)
  `(:table (:title ,(attr a "title"))
           ,@(let ((rest (rest children)))
               (cons (cons :row (first rest))
                     (rest rest)))))

(define-transform "row" (a children)
  (declare (ignore a))
  `(:row ,@children))

(define-transform "cell" (a children)
  (declare (ignore a))
  `(:cell ,(first children)))

;;; Misc

(define-transform "div" (a children)
  (declare (ignore a))
  children)

(define-transform "leq" (a children)
  (declare (ignore a children))
  (list :leq))

(define-transform "neq" (a children)
  (declare (ignore a children))
  (list :neq))

(define-transform "vert" (a children)
  (declare (ignore a children))
  (list :pipe))

(define-transform "ldots" (a children)
  (declare (ignore a children))
  (list :ellipsis))

(define-transform "CRLF" (a children)
  (declare (ignore a children))
  (list :newline-arrow))

(define-transform "keyword" (a children)
  (declare (ignore a))
  `(:keyword ,(first children)))

;;; Relations

(define-transform "EV" (a children)
  (declare (ignore a children))
  (list :right-arrow))

(define-transform "OV" (a children)
  (declare (ignore a children))
  (list :or))

(define-transform "NV" (a children)
  (declare (ignore a children))
  (list :not))

(define-transform "EQ" (a children)
  (declare (ignore a children))
  (list :equiv))

;;; Contexts

(define-transform "OUT" (a children)
  (declare (ignore a children))
  (list :output))

(define-transform "IN" (a children)
  (declare (ignore a))
  `(:input ,(first children)))

;;; BNF notation

(define-transform "more" (a children)
  `(:bnf-more))

(define-transform "star" (a children)
  `(:bnf-star ,(first children)))

(define-transform "paren" (a children)
  `(:bnf-paren ,@(rest children)))

(define-transform "lparen" (a children)
  `(:bnf-lparen))

(define-transform "rparen" (a children)
  `(:bnf-rparen))

(define-transform "xparen" (a children)
  `(:more (:bnf-lparen)))

(define-transform "brac" (a children)
  `(:bnf-brac ,@(rest children)))

(define-transform "lbrac" (a children)
  `(:bnf-lbrac))

(define-transform "rbrac" (a children)
  `(:bnf-rbrac))

(define-transform "xbrac" (a children)
  `(:more (:bnf-lbrac)))

(define-transform "curly" (a children)
  `(:bnf-curly ,@(rest children)))

(define-transform "lcurly" (a children)
  `(:bnf-lcurly))

(define-transform "rcurly" (a children)
  `(:bnf-rcurly))

(define-transform "xcurly" (a children)
  `(:more (:bnf-lcurly)))

(define-transform "down" (a children)
  `(:down))

;;; Abbreviations

(define-transform "t" (a children)
  (declare (ignore a children))
  (list :clref "t"))

;;; Glossary

(define-transform "meaning" (a children)
  (declare (ignore a))
  `(:sub ,(first children)))

;;; Removals

(defmacro define-null-transform (name)
  `(define-transform ,name (a children)
     (declare (ignore a children))
     nil))

(define-null-transform "bullet")
(define-null-transform "noindent")

;;; Interface

(defun remove-nil (atom-or-list)
  (if (atom atom-or-list)
      atom-or-list
      (remove-if #'null
                 (loop for elem in atom-or-list collecting
                       (remove-nil elem)))))

(defun post-transform-cleanup (data)
  (if (atom data)
      (if (equal data "")
          nil
          data)
      (let* ((spliced-list
               (if (and (eql (length data) 1)
                        (listp (first data)))
                   (first data)
                   data)))
        (loop for elem in spliced-list collecting
          (post-transform-cleanup elem)))))

(defun tex->lisp (node)
  (remove-nil (post-transform-cleanup (transform node))))
