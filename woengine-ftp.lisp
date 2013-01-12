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

(defmethod initialize-empty ((storage ftp-storage))
  (ftp:with-ftp-connection (conn :hostname (host storage)
				 :username (username storage)
				 :password (password storage)
				 :passive-ftp-p t)
    (ftp:send-mkd-command conn (root storage))
    (ftp:send-cwd-command conn (root storage))
    (ftp:store-file conn
		    (make-string-input-stream "NIL")
		    ".woengine-hashes"
		    :type :ascii)))

(defmethod digester ((storage ftp-storage))
  (slot-value storage 'digester))

(defmethod retrieve-hashes ((storage ftp-storage))
  (let ((hashes-file (make-string-output-stream)))
    (with-ftp-storage storage
      (ftp:retrieve-file conn ".woengine-hashes" hashes-file :type :ascii))
    (read-from-string (get-output-stream-string hashes-file) nil)))

(defmethod store-hashes (storage hashes)
  (with-ftp-storage storage
    (ftp:store-file conn
		    (make-string-input-stream
		     (prin1-to-string hashes))
		    ".woengine-hashes"
		    :type :ascii)))

(defmethod retrieve-files (storage local-root paths)
  (with-ftp-storage storage
    (dolist (path paths)
      (ftp:retrieve-file conn path (merge-pathnames path local-root)))))

(defmethod store-files (storage local-root paths)
  (with-ftp-storage storage
    (dolist (path paths)
      (ftp:store-file conn (merge-pathnames path local-root) path))))

(defmethod make-remote-directories (storage paths)
  (with-ftp-storage storage
    (dolist (path paths)
      (ftp:send-mkd-command conn path))))

(defmethod delete-remote (storage paths)
  (with-ftp-storage storage
    (dolist (path paths)
      (ftp:send-dele-command conn path))))
