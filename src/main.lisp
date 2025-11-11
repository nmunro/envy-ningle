(defpackage envy-ningle
  (:use :cl)
  (:export #:build-middleware
           #:extract-middleware-config
           #:get-config
           #:missing-config-package-error
           #:missing-config-package-error-message))

(in-package envy-ningle)

(define-condition missing-config-package-error (error)
  ((message :initarg :message :reader missing-config-package-error-message))
  (:report (lambda (condition stream)
             (format stream "Missing ENVY configuration package: ~A" (missing-config-package-error-message condition)))))

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
    (unless (and middleware (listp middleware))
      (error "[envy-ningle] :middleware config missing or not a list in ~A" envy-config))

    (dolist (entry middleware)
      (unless (and (consp entry) (keywordp (car entry)))
        (error "[envy-ningle] Invalid middleware entry: ~S~%  Each middleware entry must be a list starting with a keyword (e.g. (:session) or (:static ...))" entry)))

    (handler-case
        ;; This ensures each middleware entry is quoted
        (eval `(lack.builder:builder
                 ,@middleware
                 ,app))
      (error (e)
        (format *error-output* "~&[build-middleware ERROR] ~A~%" e)
        (error e)))))

(defun get-config (setting)
  "Retrieves a configuration SETTING from the current package."
  (let* ((pkg-name (uiop:getenv "ENVY_CONFIG_PACKAGE"))
         (pkg (and pkg-name (find-package (string-upcase pkg-name)))))
    (unless pkg-name
      (error 'missing-config-package-error
             :message "The environment variable ENVY_CONFIG_PACKAGE is not set. Please set it to the name of your configuration package (e.g., BUILD-BOT/CONFIG)."))

    (unless pkg
      (error 'missing-config-package-error
             :message (format nil "The package ~S does not exist. Please check your system definition and ensure the package is loaded." pkg-name)))

    (getf (envy:config pkg) setting)))
