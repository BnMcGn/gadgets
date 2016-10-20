(in-package :cl-user)
(defpackage test-gadgets
  (:use :cl :prove :gadgets)
  (:export #:test-gadgets))
(in-package :test-gadgets)


(plan 11)

;()
#|
(ok (getf (getf *fields* :email) :viewable))
(ok (functionp (getf (getf *fields* :email) :compiled-validator)))

(initialize-user *uname* *fields*)

(let ((udata (get-user-data *uname* *fields*)))
  (is 4 (gethash '(:system :watch-level) udata))
  (is nil (gethash '(:email) udata)))

(let ((udata (get-user-visible-data *uname* *fields*)))
  (is-values (gethash '(:system :watch-level) udata) '(nil nil))
  (ok (not (gethash '(:email) udata))))

(let ((indata (make-hash-table :test #'equal)))
  (setf (gethash '(:email) indata) "asdfasdf")
  (is-error (update-from-user *uname* *fields* indata) 'simple-error)
  (setf (gethash '(:email) indata) "asdf@asd.f")
  (ok (update-from-user *uname* *fields* indata))
  (setf (hu:hget/extend indata '(:system :watch-level)) 0)
  (is-error (update-from-user *uname* *fields* indata) 'simple-error))

(let ((udata (get-user-data *uname* *fields*)))
  (is (gethash '(:email) udata) "asdf@asd.f")
  (is 4 (gethash '(:system :watch-level) udata)))
|#


(defun test-gadgets ()
  (plan 15)

  ;;symb
  (is 'cl-user::qwer (symb 'qw 'er))

  ;;sequences-start-same
  (ok (sequences-start-same "asdf" "as"))
  (ok (sequences-start-same "as" "asdf"))
  (ok (sequences-start-same "as" "as"))
  (ok (not (sequences-start-same "as" "AS")))
  (ok (not (sequences-start-same "sa" "as")))

  ;;sequence-starts-with
  (ok (sequence-starts-with "asdf" "as"))
  (ok (not (sequence-starts-with "as" "asdf")))
  (ok (sequence-starts-with "as" "as"))
  (ok (not (sequence-starts-with "as" "AS")))
  (ok (not (sequence-starts-with "sa" "as")))

  ;;sequence-ends-with
  (ok (sequence-ends-with "asdf" "df"))
  (ok (sequence-ends-with "asdf" "asdf"))
  (ok (not (sequence-ends-with "asdf" "sd")))
  (ok (not (sequence-ends-with "df" "asdf")))

  (finalize))
