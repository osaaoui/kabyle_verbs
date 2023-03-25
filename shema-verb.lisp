(defpackage :shema-verb
  (:use :cl :uiop))


(in-package :shema-verb)

(ql:quickload :str)



(defparameter *vowels* '(a e i o u))

(defun is-vowel (letter)
  (member letter *vowels*))


(defun vowel-p (char)
  (find char "aeiou" :test #'char-equal))

(defun remove-dupl-cons (word)
  (remove-duplicates (get-cons word)))


(defun get-cons (word)
  "takes a word and removes all vowels. Then it joins the chars into a string of consonants"
  (concatenate 'string (remove-if #'vowel-p (coerce word 'list))))


(defun coerce-list (word)
  (coerce word 'list))

(defun zip-consonants(consonants)
  (mapcar #'list (coerce-list(remove-dupl-cons consonants)) '(C1 C2 C3 C4 C5)))

(defun zip-list (consonants)
  (pairlis (coerce-list(remove-dupl-cons consonants))
	   '(C1 C2 C3 C4 C5)))



(defun flatten (lst)
  (cond ((null lst) nil)
	((atom lst) (list lst))
	(t (append (flatten (car lst))
		   (flatten (cdr lst))))))



(defparameter *schema* '())
(defun get-schema (word)
  (loop :for char :across word
	:when (vowel-p char)
	  :do (cons char *schema*)))
