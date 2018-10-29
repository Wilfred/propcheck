;;; ertcheck.el --- quickcheck/hypothesis style ert tests         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.11.0"))

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: testing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT extensions to bring property-based testing to elisp. Heavily
;; influenced by the wonderful Hypothesis project.

;; References:

;; https://hypothesis.works/articles/how-hypothesis-works/
;; https://hypothesis.readthedocs.io/en/latest/details.html#how-good-is-assume
;; https://github.com/HypothesisWorks/hypothesis/blob/master/guides/internals.rst

;;; Code:

(require 'dash)
(eval-when-compile
  (require 'cl))

;; A consumable sequence of bytes used to generate test values.
(defstruct
    (ertcheck-testdata
     :named
     (:constructor ertcheck-testdata
                   (&optional
                    (bytes nil))))
  bytes)

(defun ertcheck-draw-bytes (testdata num-bytes)
  "Generate NUM-BYTES of random data, write to TESTDATA and return."
  (let ((rand-bytes (--map (random 255)
                           (number-sequence 0 (1- num-bytes)))))
    (setf (ertcheck-testdata-bytes testdata)
          (-concat (ertcheck-testdata-bytes testdata)
                   rand-bytes))
    rand-bytes))

(defun ertcheck-generate-bool (testdata)
  (let ((rand-byte (car (ertcheck-draw-bytes testdata 1))))
    (not (zerop (logand rand-byte 1)))))

(defun ertcheck-generate-integer (testdata)
  (let* ((bits-in-integers (1+ (log most-positive-fixnum 2)))
         (bytes-needed (ceiling (/ bits-in-integers 8.0)))
         (rand-bytes (ertcheck-draw-bytes testdata bytes-needed))
         (result 0))
    (dolist (byte rand-bytes)
      (setq result
            (+ (* result 256)
               byte)))
    result))

(defun ertcheck-generate-list (testdata item-generator)
  "Generate a list whose items are drawn from ITEM-GENERATOR."
  (let ((result nil))
    ;; Dumb: 50% chance of making the list bigger on each draw.
    ;; See utils.py/more in Hypothesis for a smarter approach.
    (while (>= (car (ertcheck-draw-bytes testdata 1)) 128)
      (push (funcall item-generator testdata) result))
    result))

(defun ertcheck-generate-vector (testdata item-generator)
  "Generate a vector whose items are drawn from ITEM-GENERATOR."
  (apply #'vector
         (ertcheck-generate-list testdata item-generator)))

(provide 'ertcheck)
;;; ertcheck.el ends here
