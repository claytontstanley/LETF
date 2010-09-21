(require 'asdf)
(require 'cldoc)

(defun generate-docs ()
    (cldoc:extract-documentation 'cldoc:html "html"
     '("letf.lisp" "mm.lisp")
     :table-of-contents-title
     "Common Lisp Universal Documentation Generator"))
