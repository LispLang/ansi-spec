(in-package :cl-ansi-spec)

(defun cat (list-or-string)
  (if (stringp list-or-string)
      list-or-string
      (reduce #'(lambda (l r)
                  (concatenate 'string l r))
              list-or-string)))

(defgeneric tex2xml (object)
  (:documentation "Take a Plump node from the TeX source and manipulate it."))

(defmethod tex2xml ((text-node plump:text-node))
  (plump:text text-node))

(defmethod tex2xml ((vector vector))
  (reduce #'(lambda (l r)
              (concatenate 'string l r))
          (loop for elem across vector collecting
            (tex2xml elem))))

(defmethod tex2xml ((root plump:root))
  (tex2xml (plump:children root)))

(defmethod tex2xml ((element plump:element))
  (let ((name (plump:tag-name element))
        (attributes (plump:attributes element))
        (children (plump:children element)))
    (cond
      ;; Chapters
      ((equal name "beginchapter")
       (format nil "<chapter index='~A'>"
               (cat (tex2xml children))))
      ((equal name "endchapter")
       "</chapter>")
      ;; Sections
      ((equal name "beginSection")
       (format nil "<section title='~A'>"
               (cat (tex2xml children))))
      ((equal name "endSection")
       "</section>")
      ((equal name "defineSection")
       "")
      ;; Markup
      ((equal name "term")
       (format nil "<term>~A</term>"
               (cat (tex2xml children))))
      ((equal name "funref")
       (format nil "<funref>~A</funref>"
               (cat (tex2xml children))))
      ;; Lists
      ((equal name "beginlist")
       "<list>")
      ((equal name "endlist")
       "</list>")
      ((equal name "bull")
       "<bullet/>")
      ;; Misc
      ((equal name "bye")
       "")
      (t
       ;; If the tag is unknown just throw it out
       ""))))
