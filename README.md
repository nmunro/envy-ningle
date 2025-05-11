# envy-ningle 0.0.1

## Introduction

This package is designed to be a utility to the [envy](https://github.com/fukamachi/envy) package, it intends to hide the middleware configuration for [ningle](https://github.com/fukamachi/ningle). Assuming you have a configuration for `ningle` that looks something like this:

    (defconfig |sqlite|
      `(:debug T
        :middleware ((:session)
                     (:mito (:sqlite3 :database-name ,(uiop:getenv "SQLITE_DB_NAME")))
                     (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

    (defconfig |mysql|
      `(:middleware ((:session)
                     (:mito (:mysql
                             :database-name ,(uiop:native-namestring (uiop:parse-unix-namestring (uiop:getenv "MYSQL_DB_NAME")))
                             :username ,(uiop:getenv "MYSQL_USER")
                             :password ,(uiop:getenv "MYSQL_PASSWORD")
                             :host ,(uiop:getenv "MYSQL_ADDRESS")
                             :port ,(parse-integer (uiop:getenv "MYSQL_PORT"))))
                     (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

    (defconfig |postgresql|
      `(:middleware ((:session)
                     (:mito (:postgres
                             :database-name ,(uiop:native-namestring (uiop:parse-unix-namestring (uiop:getenv "POSTGRES_DB_NAME")))
                             :username ,(uiop:getenv "POSTGRES_USER")
                             :password ,(uiop:getenv "POSTGRES_PASSWORD")
                             :host ,(uiop:getenv "POSTGRES_ADDRESS")
                             :port ,(parse-integer (uiop:getenv "POSTGRES_PORT"))))
                     (:static :root ,(asdf:system-relative-pathname :ningle-tutorial-project "src/static/") :path "/public/"))))

Where you wish to define your middleware configuration in your config (not your application), this package allows you to call:

    
     (lack.builder:builder (envy-ningle:build-middleware :ningle-tutorial-project/config *app*))
     
Allowing you to use `envy` to switch configuration simply and easily (as it is intended).

## Author

nmunro

## Links

- [envy](https://github.com/fukamachi/envy)
- [ningle](https://github.com/fukamachi/ningle)

## Licence

BSD3-Clause
