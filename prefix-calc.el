;;; prefix-calc.el --- Like quick-calc but with prefix notation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <https://github.com/rfaulhaber>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: May 15, 2021
;; Modified: May 15, 2021
;; Version: 0.1.0
;; Keywords: "calc"
;; Homepage: https://github.com/rfaulhaber/prefix-calc
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

(defconst prefix-calc-ops '(+ - * /) "Supported prefix-calc operations.")

;;;###autoload
(defun prefix-calc (ins expr)
  "Does a quick calculation in the minibuffer in prefix notation."
  (interactive "P\nsPrefix calc: ")
  (let ((res (eval (prefix-calc--transform-expr expr))))
    (if ins
        (insert (format "%d" res))
      (message "Result: %s" res))))

;;;###autoload
(defun prefix-calc-on-region ()
  (interactive)
  (when (use-region-p)
    (let* ((expr (buffer-substring (region-beginning) (region-end)))
           (res (eval (prefix-calc--transform-expr expr))))
      (replace-region-contents (region-beginning) (region-end) (lambda ()
                                                                 (format "%d" res))))))

(defun prefix-calc--transform-expr (expr)
  "Convert EXPR into a quoted S-expression,where EXPR is the string input from the minibuffer."
  (let* ((str (seq-filter
               (lambda (str)
                 (not (string= str "")))
               (s-split " " expr)))
         (qs (seq-map
              (lambda (s)
                (if (member (intern-soft s) prefix-calc-ops)
                    (intern-soft s)
                  (string-to-number s)))
              str))
         (forms (seq-map 'reverse
                         (seq-reduce
                          (lambda (acc seq)
                            (if (member seq prefix-calc-ops)
                                (cons (list seq) acc)
                              (let ((lst (cons seq (car acc))))
                                (cons lst (cdr acc)))))
                          qs
                          '()))))
    (seq-reduce
     (lambda (acc seq)
       (append seq (list acc)))
     (cdr forms)
     (car forms))))

(provide 'prefix-calc)
;;; prefix-calc.el ends here
