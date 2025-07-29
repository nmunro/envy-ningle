(defpackage envy-ningle
  (:use :cl)
  (:export #:build-middleware
           #:extract-middleware-config))

(in-package envy-ningle)

(defun extract-middleware-config (env conf)
  "Returns the keyword backend and initargs from middleware in the ENVY config."
  (let ((middleware (getf (envy:config env) :middleware)))
    (loop :for mw :in middleware
          :when (and (consp mw) (eq (car mw) conf))
          :return (destructuring-bind (_ (backend &rest args)) mw
                   (values backend args)))))

(defun build-middleware (envy-config app)
  "Wraps the given APP in middleware as defined in ENVY config at runtime."
  (let* ((config (envy:config envy-config))
         (middleware (getf config :middleware)))
    (handler-case
        ;; This ensures each middleware entry is quoted
        (eval `(lack.builder:builder
                 ,@(mapcar (lambda (entry) `',entry) middleware)
                 ,app))
      (error (e)
        (format *error-output* "~&[build-middleware ERROR] ~A~%" e)
        (error e)))))
