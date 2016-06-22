(in-package #:gadgets)


#|
;;;;;;;;;;;;

   recompile

   project to automatically recompile chunks of code when their specified
   dependencies get recompiled.

;;;;;;;;;;;;
|#

(defparameter *recomp-by-target* (make-hash-table))
(defparameter *recomp-by-label* (make-hash-table))

(defun send-recompiled-signal (names)
  (dolist (n names)
    (dolist (lab (gethash n *recomp-by-target*))
      (eval (gethash lab *recomp-by-label*)))))

(defmacro ensure-member (loc item)
  (once-only (item)
    `(unless (member ,item ,loc)
       (setf ,loc (cons ,item ,loc)))))

(defun register-recompile-watcher (label code dependencies)
  (setf (gethash label *recomp-by-label*) code)
  (dolist (dep dependencies)
    (ensure-member (gethash dep *recomp-by-target*) label)))


(defmacro watched-for-recompile (&body body)
  `(progn
     ,@body
     (send-recompiled-signal
      ,(collecting
        (do-window ((def name) (flatten body))
          (when (member def '(defun defmacro defparameter))
            (collect name)))))))

(defmacro recompile-watcher ((label &rest dependencies) &body body)
  (register-recompile-watcher label body dependencies)
  body)
