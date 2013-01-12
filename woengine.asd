(defsystem woengine
  :depends-on ("ftp" "ironclad")
  :serial t
  :components ((:file "woengine")
	       (:file "woengine-ftp")))

