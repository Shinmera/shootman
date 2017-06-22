(asdf:defsystem shootman
  :components ((:file "package")
               (:file "objects")
               (:file "map")
               (:file "gun")
               (:file "enemy")
               (:file "player")
               (:file "main"))
  :depends-on (:trial))
