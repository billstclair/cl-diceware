; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Diceware in Lisp
;;;

(cl:in-package :cl-diceware)

(defparameter *diceware-hash* nil)

(defun diceware-gethash (key)
  "Get the Diceware word for KEY, which should be a
five-digit (base 10) integer, with digits from 1 to 6."
  (let ((hash (maybe-init-diceware-hash)))
    (gethash key hash)))

(defun random-diceware-key ()
  "Compute a random Diceware key, a five-digit (base 10) integer
with digits from 1 to 6."
  (let ((num (random-integer (expt 6 5)))
        (key 0))
    (dotimes (i 5)
      (multiple-value-bind (quot rem) (floor num 6)
        (setf key (+ (* key 10) (1+ rem))
              num quot)))
    key))

(defun random-word ()
  "Return a random Diceware word."
  (values (diceware-gethash (random-diceware-key))))

(defun random-words (count)
  "Return a list of COUNT random Diceware words."
  (with-/dev/random ()
    (loop for i from 0 below count collect (random-word))))

(defun random-words-string (&optional (count 5))
  "Return a string containing COUNT random Diceware words, separated by spaces.
COUNT default to 5."
  (with-output-to-string (s)
    (with-/dev/random ()
      (loop for i from 0 below count do
           (unless (eql i 0) (write-char #\space s))
           (write-string (random-word) s)))))

(defun init-diceware-hash ()
  "Initialize and return `*diceware-hash*'."
  (let ((hash (make-hash-table :test 'eql)))
    (loop for (key value) on *diceware-words* by #'cddr
       do (setf (gethash key hash) value))
    (setf *diceware-hash* hash)))

(defun maybe-init-diceware-hash ()
  "Initialize `*diceware-hash*', if its null, and return it."
  (or *diceware-hash*
      (init-diceware-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2015 Bill St. Clair <billstclair@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
