(asdf:defsystem shootman
  :components ((:file "package")
               (:file "objects")
               (:file "main"))
  :depends-on (:trial))
