(in-package #:gadgets)

;;;Collection of tools for tracing symbols in packages.

(defun ensure-package (item)
  (if (packagep item) item
      (find-package item)))

(defun sym-sort (items)
  (sort items #'string<))

(defun generic-p (symb)
  (subtypep (type-of (fboundp symb)) 'generic-function))

(defun class-p (symb)
  ;;FIXME: Are all typedefs also classes?
  (or (is-type symb) (subtypep symb 'class)))

#+ccl
(defun is-type (symb)
  (ccl:type-specifier-p symb))

(defun package-symbols (&rest packages)
  (sym-sort
   (collecting
     (dolist (p packages)
       (do-symbols (sym (ensure-package p))
         (collect sym))))))

(defun package-exported-symbols (&rest packages)
  (sym-sort
   (collecting-set ()
     (dolist (p packages)
       (do-external-symbols (sym (ensure-package p))
         (collect sym))))))

(defun package-internal-symbols (package)
  (sym-sort
   (set-difference
    (package-symbols package)
    (package-exported-symbols package))))

(defun package-own-symbols (package)
  (sym-sort
   (set-difference
    (package-symbols package)
    (apply #'package-exported-symbols
           (package-use-list (ensure-package package))))))

(defun package-own-internal-symbols (package)
  (sym-sort
   (set-difference
    (package-own-symbols package)
    (package-exported-symbols package))))

(defun package-not-imported (package)
  "List symbols within a package that maybe should have been imported from another package."
  (let ((packs
          (collecting-hash-table (:test #'equal)
            (dolist (pack (list-all-packages))
              (unless (or (eq pack (find-package 'keyword))
                          (sequence-starts-with "PS-" (package-name pack)))
                (do-external-symbols (sym pack)
                  (collect (string-upcase (mkstr sym)) pack)))))))
    (dolist  (sym (package-own-internal-symbols package))
      (awhen (gethash (string-upcase (mkstr sym)) packs)
             (display-symbol sym)
             (print (mapcar (compose #'string-downcase #'package-name) it))))
    packs))

(defun make-symbol-flags (sym)
  (let ((preds
          (list #'boundp #'fboundp #'generic-p #'class-p #'is-type
                #'macro-function #'special-operator-p #'packagep))
        (flags '(#\b #\f #\g #\c #\t #\m #\s #\p)))
    (coerce
     (loop for p in preds
           for f in flags
           collect (if (funcall p sym) f #\-))
     'string)))

(defun display-symbol (sym)
  (format t "~&~35a  ~a  ~a"
          (string-downcase (mkstr sym))
          (make-symbol-flags sym)
          (string-downcase (package-name (symbol-package sym)))))
