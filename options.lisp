(asdf:load-system :unix-opts)

	(opts:define-opts
	 (:name :width
	  :description "width of the grid"
	  :long "width"
	  :arg-parser #'parse-integer) ;; <- takes an argument
	 (:name :height
	  :description "height of the grid"
	  :long "height"
	  :arg-parser #'parse-integer) ;; <- takes an argument
	 (:name :help
	  :description "show this help message and exit"
	  :short #\h
	  :long "help"))

(defun unknown-option (condition)
 (format t "warning: ~s option is unknown!~%" (opts:option condition))
 (invoke-restart 'opts:skip-option)
 )

(defun game (a b)
(format t "~d ~d ~%" a b)
 )

	(defmacro when-option ((options opt) &body body)
	 `(let ((it (getf ,options ,opt)))
		 (when it
		  ,@body)))

	 (multiple-value-bind (options)
	  ;errors
	  (handler-case
	   (handler-bind ((opts:unknown-option #'unknown-option))
	    (opts:get-opts))
	   (opts:missing-arg (condition)
	    (format t "fatal: option ~s needs an argument!~%"
	     (opts:option condition)))
	   (opts:arg-parser-failed (condition)
	    (format t "fatal: cannot parse ~s as argument of ~s~%"
	     (opts:raw-arg condition)
	     (opts:option condition))))
	  ;normal cases
	  (when-option (options :help)
	   ;   "help"
	   (opts:describe
	    :prefix "The best program's usage"
	    :suffix "type both width and height please"
	    :usage-of "gof.lisp")
(opts:exit 1))
	(if (and (getf options :width) (getf options :height))
	 (game (getf options :width) (getf options :height))
	  (opts:describe
	   :prefix "The best program's usage"
	   :suffix "type both width and height please"
	   :usage-of "gof.lisp")))

