(require 'asdf)

;if we're using the new asdf compiler, disable-output-translations so that compiled fasl files
;reside next to the source files (like the older asdf compiler did)
(in-package :asdf)
(if (fboundp  'disable-output-translations)
    (disable-output-translations))

;jump back in to the default common-lisp-user package
(in-package :common-lisp-user)

(load (format nil "~a/cldoc/cldoc.asd"
	(get-pandoric #'args 'platform)))

(require 'cldoc)

;macroexpand the 'methods' macro to grab the documentation for each of the methods
(cludg::define-descriptor-handler METHODS (form)
  "methods"
  (values nil :restart (list (let ((*print-case* :upcase))
			       (macroexpand-1 form)))))

(defun generate-docs ()
    (cldoc:extract-documentation 'cldoc:html "html"
     '("letf.lisp" "mm.lisp")
     :table-of-contents-title
     "Common Lisp Universal Documentation Generator"))
