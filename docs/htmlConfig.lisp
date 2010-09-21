(require 'asdf)
(asdf:disable-output-translations)

(load (format nil "~a/cldoc/cldoc.asd"
	(get-pandoric #'args 'platform)))
(require 'cldoc)

(defun generate-docs ()
    (cldoc:extract-documentation 'cldoc:html "html"
     '("letf.lisp" "mm.lisp")
     :table-of-contents-title
     "Common Lisp Universal Documentation Generator"))
