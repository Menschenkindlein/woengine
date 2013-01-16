(defpackage #:woengine
  (:use #:cl)
  (:export #:sync
           #:initialize-empty
           #:ftp-storage))

(in-package #:woengine)

;;;; API

(defgeneric digester (storage)
  (:documentation
   "Returns preferable digester.
Possible values :MD5, :SHA1 and other, supported by `ironclad'."))

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

;;;; Local actions

(defun directory-hash (pathname digester)
  "Returns all subdirectories and files with hashes in directory."
  (labels ((dir-hash-rec (path dig local)
             (if (cl-fad:directory-pathname-p path)
                 (cons (list (enough-namestring path local))
                       (loop for pth in (cl-fad:list-directory path)
                          appending
                            (dir-hash-rec pth digester local)))
                 (list (list (enough-namestring path local)
                             (unless (cl-fad:directory-pathname-p path)
                               (ironclad:digest-file digester path)))))))
    (loop for path in (cl-fad:list-directory pathname)
       appending
         (dir-hash-rec path digester (truename pathname)))))

(defun delete-local (local paths)
  (dolist (path paths)
    (setf path (merge-pathnames path local))
    (if (cl-fad:directory-pathname-p path)
        (cl-fad:delete-directory-and-files path)
        (delete-file path))))

(defun make-directories (local paths)
  (dolist (path paths)
    (ensure-directories-exist
     (merge-pathnames path local))))

;;;; Engine

(defun sync (storage local &optional restore)
  "Computes digests, finds diffs and applies them to remote copy.
Or restores local copy from remote one."
  (setf local (truename local))
  (let* ((local-hashes
          (directory-hash local (digester storage)))
         (remote-hashes (retrieve-hashes storage))
         (to-copy (mapcar #'car
                          (set-difference
                           (if restore remote-hashes local-hashes)
                           (if restore local-hashes  remote-hashes)
                           :test #'equalp)))
         (to-delete (append
                     (intersection to-copy
                                   (mapcar #'car
                                           (if restore
                                               local-hashes
                                               remote-hashes))
                                   :test #'string=)
                     (sort
                      (mapcar #'car
                              (set-difference
                               (if restore local-hashes  remote-hashes)
                               (if restore remote-hashes local-hashes)
                               :test #'string= :key #'car))
                      #'string>))))
    (when to-delete
      (format t "Deleting ~:[remote~;local~] paths:~{~%~a~}~%~%"
              restore to-delete)
      (funcall (if restore #'delete-local #'delete-remote)
               (if restore local          storage)
               to-delete))
    (let ((dirs-to-copy
           (sort (remove-if-not #'cl-fad:directory-pathname-p to-copy)
                 #'string<))
          (files-to-copy (remove-if #'cl-fad:directory-pathname-p to-copy)))
      (when dirs-to-copy
        (format t "Creating ~:[remote~;local~] directories:~{~%~a~}~%~%"
                restore dirs-to-copy)
        (funcall (if restore #'make-directories #'make-remote-directories)
                 (if restore local              storage)
                 dirs-to-copy))
      (format t "~:[Remote~;Local~] directories are up to date.~%~%"
              restore)
      (when files-to-copy
        (format t "~:[Storing local~;Retrieving remote~] files:~{~%~a~}~%~%"
                restore files-to-copy)
        (funcall (if restore #'retrieve-files #'store-files)
                 storage local files-to-copy))
      (format t "~:[Remote~;Local~] files are up to date.~%~%"
              restore))
  (unless restore (store-hashes storage local-hashes)))
  (format t "Synchronization is successful!~%~%"))
