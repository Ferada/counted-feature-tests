;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

;;; parts of this system are taken from SBCL:

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package #:cl-user)

(asdf:defsystem #:counted-feature-tests
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :description "Extended feature tests syntax."
  :long-description "Extends standard feature tests with numeric prefix."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :licence "Public Domain"
  :components ((:file "syntax"))
  :in-order-to ((asdf:test-op (asdf:test-op #:counted-feature-tests-tests)))
  :perform (asdf:test-op :after (operation component)
             (funcall (intern (symbol-name '#:run!) '#:fiveam)
                      (intern (symbol-name '#:counted-feature-tests)
                              '#:counted-feature-tests-tests))))

(asdf:defsystem #:counted-feature-tests-tests
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :depends-on (#:counted-feature-tests #:fiveam)
  :components ((:file "tests")))
