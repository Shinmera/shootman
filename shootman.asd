(asdf:defsystem shootman
  :components ((:file "package")
               (:file "objects")
               (:file "map")
               (:file "collision")
               (:file "gun")
               (:file "enemy")
               (:file "player")
               (:file "main"))
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :harmony-simple)
  :build-operation "deploy-op"
  :build-pathname "shootman"
  :entry-point "shootman:launch")
