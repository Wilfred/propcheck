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

(defvar ertcheck-max-examples 100)
;; What does hypothesis do?
(defvar ertcheck-max-shrinks 50)

;; A consumable sequence of bytes used to generate test values.
(defstruct
    (ertcheck-testdata
     :named
     (:constructor ertcheck-testdata
                   (&optional
                    (bytes nil)
                    (i 0)
                    (blocks nil)
                    (frozen nil))))
  bytes
  i
  blocks
  frozen)

(defun ertcheck-draw-bytes (testdata num-bytes)
  "Get NUM-BYTES of random data (generating if necessary), write
it to TESTDATA and return it."
  (let* ((i (ertcheck-testdata-i testdata))
         (new-i (+ i num-bytes)))
    ;; Update i to record our position in the bytes.
    (setf (ertcheck-testdata-i testdata) new-i)
    ;; Store which regions of data were accessed, as inclusive ranges.
    (push (cons i (1- new-i)) (ertcheck-testdata-blocks testdata))

    (if (ertcheck-testdata-frozen testdata)
        ;; TESTDATA was previously generated, just return the bytes we
        ;; generated in the past.
        (->> (ertcheck-testdata-bytes testdata)
             (-drop i)
             (-take num-bytes))

      ;; We're currently generating data, add it to the existing
      ;; bytes.
      (let (rand-bytes)
        (dotimes (_ num-bytes)
          (push (random 255) rand-bytes))
        (setf (ertcheck-testdata-bytes testdata)
              (-concat (ertcheck-testdata-bytes testdata)
                       rand-bytes))
        rand-bytes))))

(defun ertcheck-freeze (testdata)
  "Return a frozen copy of TESTDATA, which we can use to shrink."
  (let ((i (ertcheck-testdata-i testdata))
        (bytes (ertcheck-testdata-bytes testdata)))
    (ertcheck-testdata (-take i bytes) 0 t)))

(defun ertcheck-testdata-set-byte (testdata i value)
  "Return a copy of TESTDATA with byte I set to VALUE."
  (let ((bytes (ertcheck-testdata-bytes testdata)))
    (ertcheck-testdata
     (-replace-at i value bytes)
     (ertcheck-testdata-i testdata)
     (ertcheck-testdata-frozen testdata)
     (ertcheck-testdata-blocks testdata))))

(defun ertcheck-shrink (testdata predicate)
  "Attempt to find a smaller version of TESTDATA where predicate
still returns t. PREDICATE should be a function that takes a
`ertcheck-testdata' and returns nil or t."
  (-> testdata
      (ertcheck-shrink--zero predicate)
      (ertcheck-shrink--divide predicate)
      (ertcheck-shrink--decrement predicate)))

(defun ertcheck-shrink--zero (testdata predicate)
  "Shrink TESTDATA by zeroing bytes."
  (let ((attempts 0)
        (changed t))
    (while (and changed (< attempts ertcheck-max-shrinks))
      (setq changed nil)

      (--each-indexed (ertcheck-testdata-bytes testdata)
        (unless (zerop it)
          (cl-incf attempts)
          (let ((new-testdata
                 (ertcheck-testdata-set-byte testdata it-index 0)))
            (setq changed (funcall predicate new-testdata))
            (when changed
              (setq testdata (ertcheck-freeze new-testdata))))))))
  testdata)

(defun ertcheck-shrink--divide (testdata predicate)
  "Attempt to shrink TESTDATA by halving each byte."
  (let ((attempts 0)
        (changed t))
    (while (and changed (< attempts ertcheck-max-shrinks))
      (setq changed nil)

      (--each-indexed (ertcheck-testdata-bytes testdata)
        (unless (or (eq it 0) (eq it 1))
          (cl-incf attempts)
          (let ((new-testdata
                 (ertcheck-testdata-set-byte testdata it-index (/ it 2))))
            (setq changed (funcall predicate new-testdata))
            (when changed
              (setq testdata (ertcheck-freeze new-testdata))))))))
  testdata)

(defun ertcheck-shrink--decrement (testdata predicate)
  "Attempt to shrink TESTDATA by decrementing each byte."
  (let ((attempts 0)
        (changed t)
        (bytes (ertcheck-testdata-bytes testdata)))
    (while (and changed (< attempts ertcheck-max-shrinks))
      (setq changed nil)

      (--each-indexed bytes
        (unless (zerop it)
          (cl-incf attempts)
          (let ((new-testdata
                 (ertcheck-testdata-set-byte testdata it-index (1- it))))
            ;; Try incrementing the next byte too.
            (when (and (eq it 1) (> (length bytes) (1+ it-index)))
              (setq new-testdata
                    (ertcheck-testdata-set-byte
                     new-testdata
                     (1+ it-index)
                     (1+ (nth (1+ it-index) bytes)))))
            (setq changed (funcall predicate new-testdata))
            (when changed
              (setq testdata (ertcheck-freeze new-testdata))))))))
  testdata)

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

(defun ertcheck-generate-ascii-char (testdata)
  "Generate a number that's an ASCII char.
Note that elisp does not have a separate character type."
  ;; between 32 and 126
  (let* ((rand-bytes (ertcheck-draw-bytes testdata 1))
         (byte (car rand-bytes))
         (min-ascii 32)
         (max-ascii 126)
         (ascii-range (- max-ascii min-ascii)))
    (+ min-ascii (mod byte ascii-range))))

;; TODO: circular lists, improprer lists/trees.
(defun ertcheck-generate-list (testdata item-generator)
  "Generate a list whose items are drawn from ITEM-GENERATOR."
  (let ((result nil))
    ;; Dumb: 75% chance of making the list bigger on each draw.
    ;; See utils.py/more in Hypothesis for a smarter approach.
    (while (>= (car (ertcheck-draw-bytes testdata 1)) 64)
      (push (funcall item-generator testdata) result))
    result))

(defun ertcheck-generate-vector (testdata item-generator)
  "Generate a vector whose items are drawn from ITEM-GENERATOR."
  (apply #'vector
         (ertcheck-generate-list testdata item-generator)))

(defun ertcheck-generate-string (testdata)
  "Generate a string using TESTDATA."
  (let ((chars nil))
    ;; Dumb: 75% chance of making the string bigger on each draw.
    ;; TODO: see what hypothesis does
    ;; TODO: multibyte support, key sequence support
    (while (>= (car (ertcheck-draw-bytes testdata 1)) 64)
      (push
       (ertcheck-generate-ascii-char testdata)
       chars))
    (concat chars)))

(provide 'ertcheck)
;;; ertcheck.el ends here
