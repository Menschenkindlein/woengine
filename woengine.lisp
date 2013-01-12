(defpackage #:woengine
  (:use #:cl)
  (:export #:woengine
	   #:initialize-empty
	   #:ftp-storage))

(in-package #:woengine)

;;;; API

(defgeneric digester (storage)
  (:documentation
   "Returns preferable digester.
Posiible values :MD5, :SHA1 and other, supported by `ironclad'."))

(defgeneric initialize-empty (storage)
  (:documentation "Initializes empty storage. If needed.")
  (:method ((storage t)) nil))

(defgeneric retrieve-hashes (storage)
  (:documentation
   "Retrieves hashes in the same form as STORE-HASHES."))

(defgeneric store-hashes (storage hashes)
  (:documentation
   "Stores hashes, given as list."))

(defgeneric retrieve-files (storage local-root paths)
  (:documentation
   "Retrieves files.
Directories assumed to be existing."))

(defgeneric store-files (storage local-root local-paths)
  (:documentation
   "Stores files, given by relative path in the same relative remote path.
Overwrites existing files."))

(defgeneric make-remote-directories (storage paths)
  (:documentation
   "Makes remote directories."))

(defgeneric delete-remote (storage remote)
  (:documentation
   "Deletes remote files and directories"))

;;;; Utils

(defun directoryp (filespec)
  "Returns NIL if pathspec does not look like directory and T otherwise."
  (string= (file-namestring filespec) ""))

(defun all-files-directory (pathname)
  "Recursively returns all files in directory."
  (if (directoryp pathname)
      (cons (truename pathname)
            (loop for path in (directory (merge-pathnames
                                          (make-pathname :name :wild
                                                         :type :wild)
                                          (truename pathname)))
               appending
                 (all-files-directory path)))
      (list (truename pathname))))

(defun delete-local (local paths)
  (when paths
    (progn
      (format t "Deleting local paths: ~{~%~a~}~%~%" paths)
      (dolist (path paths)
	(setf path (merge-pathnames path local))
	(if (directoryp path)
	    (sb-ext:delete-directory path)
	    (delete-file path))))))

(defun make-directories (local paths)
  (if paths
      (progn
	(format t "Making local directories: ~{~%~a~}~%~%" paths)
	(dolist (path paths)
	  (ensure-directories-exist
	   (merge-pathnames path local))))
      (format t "Local directories are up to date.~%")))

;;;; Engine

(defun woengine (storage local &optional restore)
  "Computes digests, finds diffs and applies them to remote copy.
Or restores local copy from remote one."
  (setf local (truename local))
  (let ((local-hashes
         (mapcar (lambda (file)
                   (list (enough-namestring file local)
                         (unless (directoryp file)
                           (ironclad:digest-file (digester storage) file))))
                 (remove local (all-files-directory local) :test #'equal)))
	(remote-hashes (retrieve-hashes storage)))
    (let ((to-copy (mapcar #'car
			   (set-difference
			    (if restore remote-hashes local-hashes)
			    (if restore local-hashes  remote-hashes)
			    :test #'equalp)))
	  (to-delete (mapcar #'car
			   (set-difference
			    (if restore local-hashes  remote-hashes)
			    (if restore remote-hashes local-hashes)
			    :test #'string= :key #'car))))
      (if restore
	  (progn (delete-local local (sort to-delete #'string>))
		 (make-directories local
				   (sort (remove-if-not #'directoryp
							to-copy)
					 #'string<))
		 (retrieve-files storage local (remove-if #'directoryp
							  to-copy)))
	  (progn (delete-remote storage (sort to-delete #'string>))
		 (make-remote-directories
		  storage
		  (sort (remove-if-not #'directoryp to-copy) #'string<))
		 (store-files storage local (remove-if #'directoryp to-copy))
		 (store-hashes storage local-hashes)))))
  (format t "Synchronization is successful!~%~%"))
