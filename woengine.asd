(defsystem woengine
  :depends-on ("ftp" "ironclad" "cl-fad")
  :serial t
  :components ((:file "woengine")
               (:file "woengine-ftp")))

