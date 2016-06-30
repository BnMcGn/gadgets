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

(defmacro watch-for-recompile (&body body)
  (let ((names (collecting
                   (do-window ((def name) (flatten body))
                     (when (member def '(defun defmacro defparameter))
                       (collect name))))))
    `(prog1
         (progn ,@body)
       (request-watch-on-names ',names)
       (send-recompiled-signal ',names))))

(defmacro watch-for-recompile/auto-watcher (label &body body)
  `(watch-for-recompile
     (dependency-auto-watcher ,label
       ,@body)))

(defmacro dependency-watcher ((label &rest dependencies) &body body)
  (register-recompile-watcher label `(progn ,@body) dependencies)
  `(progn ,@body))

(defvar *recomp-targets-to-watch*)

(defun request-watch-on-names (target-names)
  (when (boundp '*recomp-targets-to-watch*)
    (dolist (tnam target-names)
      (ensure-member *recomp-targets-to-watch* tnam))))

(defmacro dependency-auto-watcher (label &body body)
  `(progn
     ,@(loop
          for code in body
          for i from 0
          collect
            `(let ((*recomp-targets-to-watch* nil))
               ,code
               (register-recompile-watcher
                ',(let ((*package* (symbol-package label)))
                       (symb label i))
                '(progn ,code)
                *recomp-targets-to-watch*)))))
