;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:counted-feature-tests-tests
  (:use #:cl #:counted-feature-tests #:fiveam))

(in-package #:counted-feature-tests-tests)

(file-enable-counted-feature-tests-syntax)

(def-suite counted-feature-tests)

(in-suite counted-feature-tests)

(def-test useless.zero ()
  "Zero prefix is no-op."
  (is (eq 'useless #0+is 'useless)))

(def-test or-other-stuff ()
  "Always skip next two forms with (OR)."
  (is (equal '(other stuff)
             (list #2+(or) :a-keyword-argument and-its-value
                   'other 'stuff))))

(def-test useless.and ()
  "Always read next form with (AND) and one prefix."
  (is (eql 42 #1+(and) 42)))
