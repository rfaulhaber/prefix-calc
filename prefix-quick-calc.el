;;; prefix-quick-calc.el --- Like quick-calc but with prefix notation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <https://github.com/rfaulhaber>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: May 15, 2021
;; Modified: May 15, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/rfaulhaber/prefix-quick-calc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Like quick-calc but with prefix notation.
;;
;;  Do prefix calculations in the minibuffer. Each operator has an indefinite
;;  arity and converts each expression into an S-expression. So
;;
;;  + 1 2 3 - 4 5
;;
;;  becomes
;;
;;  (+ 1 2 3 (- 4 5))
;;
;;
;;; Code:

(defun prefix-quick-calc (expr)
  "Does a quick calculation in the minibuffer in prefix notation."
  (interactive "sPrefix quick calc: ")
  (message "Result: %s" (eval (prefix-quick-calc--transform-expr expr))))

(defun prefix-quick-calc--transform-expr (expr)
  "Convert EXPR into a quoted S-expression,where EXPR is the string input from the minibuffer."
  (let* ((str (seq-filter
               (lambda (str)
                 (not (string= str "")))
               (s-split " " expr)))
         (qs (mapcar
              (lambda (s)
                (pcase s
                  ("+" '+)
                  ("-" '-)
                  ("*" '*)
                  ("/" '/)
                  (_ (string-to-number s))))
              str))
         (forms (seq-map 'reverse
                         (seq-reduce
                          (lambda (acc seq)
                            (if (member seq '(+ - * /))
                                (cons (list seq) acc)
                              (let ((lst (cons seq (car acc))))
                                (cons lst (cdr acc)))
                              ))
                          qs '()))))
    (seq-reduce
     (lambda (acc seq)
       (append seq (list acc)))
     (cdr forms) (car forms))))

(provide 'prefix-quick-calc)
;;; prefix-quick-calc.el ends here
