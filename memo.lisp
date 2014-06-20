;;;; -*- common-lisp-style: "drurowin" -*-
;;;; memo.lisp
;;; Author:
;;
;; Lucien Pullen <drurowin@gmail.com>
;;
;;; Maintainer:
;;
;; Lucien Pullen <drurowin@gmail.com>
;;
;;; Synopsis:
;;
;; Function memoization, including memoizing existing functions
;; (`with-memoization') locally.  Memoization cache is handled by the
;; garbage collector, so as long as the values get garbage collected,
;; the memoization table shouldn't use all available memory.
;;
;;; Copyright:
;;
;; Copyright (C) 2014 Lucien Pullen
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
(cl:in-package :cl-user)

(defpackage :org.drurowin.memo
  (:use #:cl)
  (:export #:defmemo
           #:lambda/memo
           #:with-memoization)
  (:local-nicknames (#:sequence #:org.drurowin.sequence))
  (:documentation "Function memoization."))

(in-package :org.drurowin.memo)

;; this comes from SBCL sources
(defun parse-body (body &key (doc-string-allowed t) (toplevel nil))
  "Given a sequence of declarations (and possible a documentation
string) followed by other forms (as occurs in the bodies of DEFUN,
DEFMACRO, etc.) return (VALUES FORMS DECLS DOC), where DECLS holds
declarations, DOC holds a doc string (or NIL if none), and FORMS holds
the other forms.

If DOC-STRING-ALLOWED is NIL, then no forms will be treated as
documentation strings."
  (let* ((decls (list nil))
         (decls-tail decls)
         (forms body)
         (doc nil))
    (flet ((doc-string-p (form remaining-forms)
             (if (and (stringp form)
                      doc-string-allowed)
                 ;; ANSI 3.4.11 explicitly requires that a doc string be
                 ;; followed by another form (either an ordinary form or
                 ;; a declaration). Hence:
                 (if (and remaining-forms doc)
                     ;; ANSI 3.4.11 says that the consequences of
                     ;; duplicate doc strings are unspecified. That's
                     ;; probably not something the programmer
                     ;; intends. We raise an error so that this won't
                     ;; pass unnoticed.
                     (error "Duplicate doc string: ~S" form)
                     t)))
           (declaration-p (form)
             (when (consp form)
               (let ((name (car form)))
                 (case name
                   ((declare) t)
                   ((declaim)
                      (unless toplevel
                        ;; Technically legal, but probably not what the
                        ;; programmer meant to do.
                        (warn "DECLAIM where DECLARE was probably meant.")))
                   (t nil))))))
      (tagbody
       :again
         (if forms
             (let ((form1 (first forms)))
               (if (doc-string-p form1 (rest forms))
                   (setq doc form1)
                   (if (declaration-p form1)
                       ;; Here's a little optimization to skip the
                       ;; (NREVERSE DECLS) at the end. I don't know if
                       ;; SBCL has RPLACD at this point during
                       ;; bootstrap, but this would make it a bit faster
                       ;; if it did.
                       (setf (cdr decls-tail) (cons form1 nil)
                             decls-tail (last decls-tail))
                       (go :done)))
               (setq forms (rest forms))
               (go :again)))
       :done)
      (values forms (cdr decls) doc))))

(defun format-sym (control-string &rest args)
  (gensym (apply #'format nil control-string args)))

(defun frob-lambda-list (args)
  (sequence:collect (args)
    (let ((mode nil))
      (dolist (arg args (args))
        (case arg
          ((&optional &rest &key &aux &allow-other-keys)
             (setf mode arg))
          (t (case mode
               ((nil &rest)
                  (args arg))
               (&optional
                  (args (if (consp arg) (car arg) arg))
                  (if (and (consp arg) (third arg)) (args (third arg))))
               (&key
                  (args (if (consp arg)
                            (if (consp (car arg)) (cadar arg) (car arg))
                            arg))
                  (if (and (consp arg) (third arg)) (args (third arg))))
               (&allow-other-keys (warn "Parameter ~A in &ALLOW-OTHER-KEYS section." arg))
               (&aux (args (if (consp arg) (car arg) arg))))))))))

(defun memoizing-body (vars memo-table default fetch body)
  (let ((vars (if (= (length vars) 1)
                  (car vars)
                  `(list ,@vars))))
    `(let ((,fetch (gethash ,vars ,memo-table ,default)))
       (if (eq ,fetch ,default)
           (setf (gethash ,vars ,memo-table)
                 ,body)
           ,fetch))))

(defmacro with-memoization (functions &body body)
  "Memoize calls to FUNCTIONS in the lexical BODY forms.

NOTE: To invoke the memoized function in calls such as MAPCAR and
FUNCALL the function must be written in the #' form, not the ' form."
  (unless (every #'symbolp functions)
    (error 'type-error :expected-type `(every symbol)
                       :datum functions))
  (let ((bindings
          (mapcar (lambda (fn) (list fn
                                     (format-sym "~A-MEMO-TABLE" fn)
                                     (format-sym "~A-FETCH-VAR" fn)
                                     (format-sym "~A-FETCH-DEFAULT" fn)))
                  functions)))
    `(let (,@(loop :for binding :in bindings
                   :collect `(,(second binding) (make-hash-table :test 'equal :weakness :value))
                   :collect `(,(fourth binding) (cons nil nil))))
       (flet (,@(mapcar (lambda (binding)
                          `(,(first binding) (&rest args)
                            ,(memoizing-body '(args)
                                             (second binding) (fourth binding)
                                             (third binding)
                                             `(apply ',(first binding) args))))
                  bindings))
         (progn ,@body)))))

(defmacro defmemo (name args &body body)
  "Define a function NAME that memoizes results based on its arguments."
  (let ((memo-table (format-sym "~A-MEMO-TABLE" name))
        (fetch (format-sym "~A-FETCH-VAR" name))
        (default (format-sym "~A-FETCH-DEFAULT" name))
        (vars (frob-lambda-list args)))
    (multiple-value-bind (body decls doc)
        (parse-body body)
      `(let ((,memo-table (make-hash-table :test 'equal :weakness :value))
             (,default (cons nil nil)))
         (defun ,name ,args
           ,@(if doc (list doc) nil)
           ,@decls
           ,(memoizing-body vars memo-table default fetch
                            `(block ,name ,@body)))))))

(defmacro lambda/memo (args &body body)
  "Define an anonymous function that memoizes results based on its arguments."
  (let ((memo-table (gensym "LAMBDA-MEMO-TABLE"))
        (fetch (gensym "LAMBDA-FETCH"))
        (default (gensym "LAMBDA-FETCH-DEFAULT"))
        (vars (frob-lambda-list args)))
    (multiple-value-bind (body decls doc)
        (parse-body body)
      `(let ((,memo-table (make-hash-table :test 'equal :weakness :value))
             (,default (cons nil nil)))
         (lambda ,args
           ,@(if doc (list doc) nil)
           ,@decls
           ,(memoizing-body vars memo-table default fetch
                            `(block nil ,@body)))))))
