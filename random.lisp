; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Yet another cryptographically secure random number library.
;;; Uses /dev/urandom by default, but with the option to use /dev/random
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

(defvar */dev/random-stream* nil)
(defvar *remainder*)
(defvar *remainder-bits*)

(defun call-with-/dev/random (thunk)
  "Helper function for `with-dev/random'."
  (if */dev/random-stream*
      (funcall thunk */dev/random-stream*)
      (with-open-file (s (/dev/random-path) :element-type '(unsigned-byte 8))
        (let ((*/dev/random-stream* s)
              (*remainder* 0)
              (*remainder-bits* 0))
          (funcall thunk s)))))

(defun random-byte ()
  "Return one random byte."
  (with-/dev/random (s) (%random-byte s)))

(defun %random-byte (s)
  (read-byte s))

(defun %shift-in (accum fix-accum fix-accum-bits fix-accum-size value bits)
  (cond ((>= (+ fix-accum-bits bits) fix-accum-size)
         (cond ((>= bits fix-accum-size)
                (setf accum (+ (ash accum (+ fix-accum-bits bits))
                               (ash fix-accum fix-accum-bits)
                               value)
                      fix-accum 0
                      fix-accum-bits 0))
               (t (setf accum (+ (ash accum fix-accum-bits) fix-accum)
                        fix-accum value
                        fix-accum-bits bits))))
        (t (setf fix-accum (+ (ash fix-accum bits) value))
           (incf fix-accum-bits bits)))
  (values accum fix-accum fix-accum-bits))

(defun %shift-value (accum fix-accum fix-accum-bits)
  (+ (ash accum fix-accum-bits) fix-accum))

;; Enables bit accumulation with minimal bignum consing
(defmacro with-shift-in (&body body)
  (let ((accum (gensym "ACCUM"))
        (fix-accum (gensym "FIX-ACCUM"))
        (fix-accum-bits (gensym "FIX-ACCUM-BITS"))
        (fix-accum-size (integer-length most-positive-fixnum)))
    `(let ((,accum 0)
           (,fix-accum 0)
           (,fix-accum-bits 0))
       (flet ((shift-in (value bits)
                (multiple-value-setq (,accum ,fix-accum ,fix-accum-bits)
                  (%shift-in ,accum ,fix-accum ,fix-accum-bits ,fix-accum-size
                             value bits)))
              (shift-value ()
                (prog1 (%shift-value ,accum ,fix-accum, fix-accum-bits)
                  (setf ,accum 0
                        ,fix-accum 0
                        ,fix-accum-bits 0))))
         (declare (dynamic-extent #'shift-in #'shift-value))
         ,@body))))

(defun random-bits (count)
  "Return an integer containing COUNT random bits."
  (with-/dev/random (s) (%random-bits count s)))

(defun %random-bits (count s)
  (cond ((>= *remainder-bits* count)
         (prog1 (logand (1- (ash 1 count)) *remainder*)
           (setf *remainder* (ash *remainder* (- count)))
           (decf *remainder-bits* count)))
        (t (let* ((full-bytes (floor (- count *remainder-bits*) 8))
                  (last-byte-bits (- count *remainder-bits* (* 8 full-bytes))))
             (with-shift-in
               (shift-in *remainder* *remainder-bits*)
               (dotimes (i full-bytes)
                 (shift-in (%random-byte s) 8))
               (cond ((> last-byte-bits 0)
                      (let ((last-byte (%random-byte s)))
                        (shift-in (logand last-byte (1- (ash 1 last-byte-bits)))
                                  last-byte-bits)
                        (setf *remainder* (ash last-byte (- last-byte-bits))
                              *remainder-bits* (- 8 last-byte-bits))))
                     (t (setf *remainder* 0
                              *remainder-bits* 0)))
               (shift-value))))))

;; Can't just get COUNT bits and reduce to LIMIT's range.
;; That makes some values more likely than others.
;; This consumes (on average) up to twice the entropy, and time,
;; but gets a uniformly distributed result.
(defun random-integer (limit)
  "Return a random integer >= 0 and < limit. Same as `cl:random', but better randomness."
  (check-type limit (integer 0))
  (when (eql limit 0) (return-from random-integer 0))
  (with-/dev/random (s)
    (loop with count = (integer-length limit)
       for bits = (%random-bits count s)
       when (< bits limit) return bits)))

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

(defun random-bits (count)
  "(cl-random (ash 1 count))"
  (random (ash 1 count)))

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
