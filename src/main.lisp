(defpackage envy-ningle
  (:use :cl)
  (:export #:build-middleware))

(in-package envy-ningle)

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
