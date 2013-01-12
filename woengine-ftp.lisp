(in-package #:woengine)

(defclass ftp-storage ()
  ((digester :initarg :digester)
   (host :initarg :host :reader host)
   (username :initarg :username :reader username)
   (password :initarg :password :reader password)
   (root :initarg :root :reader root)))

(defmacro with-ftp-storage (storage &body body)
  `(ftp:with-ftp-connection (conn :hostname (host ,storage)
				  :username (username ,storage)
				  :password (password ,storage)
				  :passive-ftp-p t)
     (ftp:send-cwd-command conn (root ,storage))
     ,@body))

(defmethod initialize empty ((storage ftp-storage))
  (ftp:with-ftp-connection (conn :hostname (host storage)
				 :username (username storage)
				 :password (password storage)
				 :passive-ftp-p t)
    (ftp:send-mkd-command conn (root storage))
    (ftp:send-cwd-command conn (root storage))
    (ftp:store-file conn
		    (make-string-input-stream "")
		    ".woengine-hashes"
		    :type :ascii)))

(defmethod digester ((storage ftp-storage))
  (slot-value storage 'digester))

(defmethod retrieve-hashes ((storage ftp-storage))
  (with-ftp-storage storage
    (ftp:retrieve-file conn ".woengine-hashes" ".temp-woengine-hashes"))
  (let ((hashes (with-open-file (file ".temp-woengine-hashes")
		  (read file nil))))
    (delete-file ".temp-woengine-hashes")
    hashes))

(defmethod store-hashes (storage hashes)
  (with-open-file (file ".temp-woengine-hashes"
			:direction :output
			:if-does-not-exist :create)
    (print hashes file))
  (with-ftp-storage storage
    (ftp:store-file conn ".temp-woengine-hashes" ".woengine-hashes"))
  (delete-file ".temp-woengine-hashes"))

(defmethod retrieve-files (storage local-root paths)
  (if paths
      (progn
	(format t "Retrieving remote files: ~{~%~a~}~%~%" paths)
	(with-ftp-storage storage
	  (dolist (path paths)
	    (ftp:retrieve-file conn path (merge-pathnames path local-root)))))
      (format t "Local files are up to date.~%~%")))

(defmethod store-files (storage local-root paths)
  (if paths
      (progn
	(format t "Copying local files: ~{~%~a~}~%~%" paths)
	(with-ftp-storage storage
	  (dolist (path paths)
	    (ftp:store-file conn (merge-pathnames path local-root) path))))
      (format t "Remote files are up to date.~%~%")))

(defmethod make-remote-directories (storage paths)
  (if paths
      (progn
	(format t "Making remote directories: ~{~%~a~}~%~%" paths)
	(with-ftp-storage storage
	  (dolist (path paths)
	    (ftp:send-mkd-command conn path))))
      (format t "Remote directories are up to date.~%~%")))

(defmethod delete-remote (storage paths)
  (when paths
    (progn
      (format t "Deleting remote paths: ~{~%~a~}~%~%" paths)
      (with-ftp-storage storage
	(dolist (path paths)
	  (ftp:send-dele-command conn path))))))
