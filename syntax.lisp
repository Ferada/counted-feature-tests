;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:counted-feature-tests
  (:use #:cl)
  (:export
   #:featurep
   #:disable-counted-feature-tests-syntax
   #:locally-disable-counted-feature-tests-syntax
   #:enable-counted-feature-tests-syntax
   #:locally-enable-counted-feature-tests-syntax
   #:file-enable-counted-feature-tests-syntax
   #:restore-counted-feature-tests-syntax-state))

(in-package #:counted-feature-tests)

;;; the goal is to enable #+ and #- to ignore multiple expressions by
;;; supplying an additional numeric prefix

;;; zero is kind of pointless, but would be accepted, possibly with a
;;; warning, one is the default

;;; note that you can't enter a negative value in the first place, so that
;;; point is moot

;;; discarding all READ forms until the end of file should probably not
;;; be mashed together with this, as it's not an extension of the
;;; otherwise fairly obvious semantics of this extension

;;; since the number of discarded expressions must exactly match the
;;; numeric prefix, #999+(or) can not be used to achieve this


;;; as a prerequisite, let us define a function for evaluating feature
;;; expressions - should probably be a standard function as well

;;; (featurep 'x) then means roughly the same as #+x T

;; sbcl/src/code/early-extensions.lisp:963 has the implementation for SBCL
;; slightly changed for portability, i.e. MEMQ to MEMBER

;;; If X is a symbol, see whether it is present in *FEATURES*. Also
;;; handle arbitrary combinations of atoms using NOT, AND, OR.
(defun featurep (x)
  (typecase x
    (cons
     (case (car x)
       ((:not not)
        (cond
          ((cddr x)
           (error "too many subexpressions in feature expression: ~S" x))
          ((null (cdr x))
           (error "too few subexpressions in feature expression: ~S" x))
          (t (not (featurep (cadr x))))))
       ((:and and) (every #'featurep (cdr x)))
       ((:or or) (some #'featurep (cdr x)))
       (t
        (error "unknown operator in feature expression: ~S" x))))
    (symbol (not (null (member x *features* :test #'eq))))
    (t
      (error "invalid feature expression: ~S" x))))

;;; now we can use this to implement the two reader macros
;;; implementation-independently

;; sbcl/src/code/sharpm.lisp:354 has the implementation for SBCL
;; slightly changed, i.e. uses FIND-PACKAGE
;; furthermore it now just READ and discards N times instead of once

;;;; conditional compilation: the #+ and #- readmacros

(flet ((guts (stream count not-p)
         (unless (if (let ((*package* #.(find-package '#:keyword))
                           (*read-suppress* nil))
                       (featurep (read stream t nil t)))
                     (not not-p)
                     not-p)
           (let ((*read-suppress* t))
             (dotimes (i (or count 1))
               (read stream t nil t))))
         (values)))

  (defun sharp-plus (stream sub-char numarg)
    (declare (ignore sub-char))
    (guts stream numarg nil))

  (defun sharp-minus (stream sub-char numarg)
    (declare (ignore sub-char))
    (guts stream numarg t)))

(defvar *original-readtable* NIL)

(defvar *restore-counted-feature-tests-syntax* NIL)

(defmacro disable-counted-feature-tests-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-reader-syntax* NIL)
    (%disable-counted-feature-tests-syntax)))

(defmacro locally-disable-counted-feature-tests-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-counted-feature-tests-syntax)))

(defun %disable-counted-feature-tests-syntax ()
  (when *original-readtable*
    (setf *readtable* *original-readtable*
          *original-readtable* NIL))
  (values))

(defmacro enable-counted-feature-tests-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-reader-syntax* T)
    (%enable-counted-feature-tests-syntax)))

(defmacro locally-enable-counted-feature-tests-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-counted-feature-tests-syntax)))

(defmacro file-enable-counted-feature-tests-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-counted-feature-tests-syntax NIL)))

(defun %enable-counted-feature-tests-syntax (&optional (save-original-p T))
  (when save-original-p
    (setf *original-readtable* (copy-readtable)))
  (when (or (not save-original-p) *original-readtable*)
    (setf *readtable* (copy-readtable))
    (set-dispatch-macro-character #\# #\+ #'sharp-plus)
    (set-dispatch-macro-character #\# #\- #'sharp-minus))
  (values))

(defmacro restore-counted-feature-tests-syntax-state ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (if *restore-counted-feature-tests-syntax*
        (%enable-counted-feature-tests-syntax)
        (%disable-counted-feature-tests-syntax))))

;;; which is then used like follows, #- is of course analogous

;; (list #2+(or)
;;       :a-keyword-argument and-its-value
;;       'other 'stuff)
;; => (OTHER STUFF)

;; #0+is 'useless
;; => USELESS

;; #+(and) 42
;; OR #1+(and) 42
;; => 42
