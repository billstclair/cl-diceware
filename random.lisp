; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Yet another cryptographically secure random number library.
;;; Uses /dev/random, so not for when you need it quickly.
;;; Just uses cl:random in Windows.
;;;

(cl:in-package :cl-diceware)

#-windows
(progn

(defmacro with-/dev/random ((&optional (stream (gensym "STREAM"))) &body body)
  "Evaluate BODY with STREAM bound to a binary input stream to /dev/random."
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,stream)
              (declare (ignorable ,stream))
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-/dev/random #',thunk))))

(defvar */dev/random-stream* nil)

(defvar *real-random-p* :use-features
  "Control whether we use /dev/random or /dev/urandom.
See documentation for `real-random-p'.")

(defun real-random-p ()
  "True if `*real-random-p*' is true or `*real-random-p*' is :USE-FEATURES
and :CL-DICEWARE-REAL-RANDOM is on `*features*', which it will be if
you load cl-diceware.asd with a non-null value for the CL_DICEWARE_REAL_RANDOM
environment variable."
  (if (eq *real-random-p* :use-features)
      (not (null (member :cl-diceware-real-random-p *features*)))
      *real-random-p*))

(defun /dev/random-path ()
  (if (real-random-p) "/dev/random" "/dev/urandom"))

(defun call-with-/dev/random (thunk)
  "Helper function for `with-dev/random'."
  (if */dev/random-stream*
      (funcall thunk */dev/random-stream*)
      (with-open-file (s (/dev/random-path) :element-type '(unsigned-byte 8))
        (let ((*/dev/random-stream* s))
          (funcall thunk s)))))

(defun random-byte ()
  "Return one random byte."
  (with-/dev/random (s)
    (read-byte s)))

(defun random-integer (limit)
  "Return a random integer >= 0 and < limit. Same as `cl:random', but better."
  (check-type limit (integer 0))
  (let* ((bytes (ceiling (integer-length limit) 8))
         (divisor (ash 1 (* 8 bytes)))
         (sum 0))
    (with-/dev/random ()
      (dotimes (i bytes)
        (setf sum (+ (ash sum 8) (random-byte))))
      (values (floor (* limit sum) divisor)))))

)

#+windows
(progn

(defmacro with-/dev/random ((&optional stream) body)
  "Execute BODY."
  (declare (ignore stream))
  `(progn ,@body))

(defun random-byte ()
  "(cl:random 256)"
  (random 256))

(defun random-integer (limit)
  "(cl:random LIMIT)"
  (random limit))

)

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
