;;; propcheck.el --- quickcheck/hypothesis style testing for elisp         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wilfred Hughes
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

;; Property based testing for Emacs Lisp. Heavily influenced by the
;; wonderful Hypothesis project.

;; References:

;; https://hypothesis.works/articles/how-hypothesis-works/
;; https://hypothesis.readthedocs.io/en/latest/details.html#how-good-is-assume
;; https://github.com/HypothesisWorks/hypothesis/blob/master/guides/internals.rst

;;; Code:

(require 'dash)
(eval-when-compile
  (require 'cl))

(defvar propcheck-max-examples 100)
;; What values does hypothesis use?
(defvar propcheck-max-shrinks 200)
(defvar propcheck--shrinks-remaining nil)
(defvar propcheck--allow-replay nil)

(defvar propcheck--seed
  nil
  "The current seed being used to generate values.
This is a global variable so users aren't force to pass it to
generate functions.")

;; The value used to generate sample data. This is a list of bytes,
;; along with an index. When regenerating data, resetting the index to
;; 0 and calling the same generate functions will produce the same
;; values.
;;
;; Hypothesis calls this 'TestData'.
(defstruct
    (propcheck-seed
     :named
     (:constructor propcheck-seed
                   (&optional
                    (bytes nil)
                    (i 0))))
  bytes
  i)

(defun propcheck--draw-bytes (seed num-bytes)
  "Get NUM-BYTES of random data from SEED, generating if necessary.
This function modifies SEED and returns the generated bytes."
  (propcheck--sanity-check seed)
  (let* ((i (propcheck-seed-i seed))
         (bytes (propcheck-seed-bytes seed))
         (new-i (+ i num-bytes)))
    ;; Update i to record our position in the bytes.
    (setf (propcheck-seed-i seed) new-i)

    (if (<= new-i (length bytes))
        ;; SEED already has sufficient bytes, just return the
        ;; previously generated bytes.
        (->> (propcheck-seed-bytes seed)
             (-drop i)
             (-take num-bytes))

      ;; We're currently generating data, add it to the existing
      ;; bytes.
      (let (rand-bytes)
        (dotimes (_ num-bytes)
          (push (random 255) rand-bytes))
        (setf (propcheck-seed-bytes seed) (-concat bytes rand-bytes))
        rand-bytes))))

(defun propcheck--seek-start (seed)
  "Return a copy of SEED with i set to the beginning."
  (propcheck-seed (propcheck-seed-bytes seed) 0))

(defun propcheck--sanity-check (seed)
  (unless seed
    (error "seed must not be nil"))
  (if (or propcheck--shrinks-remaining propcheck--allow-replay)
      (unless (<
               (propcheck-seed-i seed)
               (length (propcheck-seed-bytes seed)))
        (error "When replaying seeds, i should be less than the bytes available"))
    (unless (=
             (propcheck-seed-i seed)
             (length (propcheck-seed-bytes seed)))
      (error "Data should be growing when finding counterexamples"))))

(defun propcheck-generate-bool ()
  (let ((rand-byte (car (propcheck--draw-bytes propcheck--seed 1))))
    (not (zerop (logand rand-byte 1)))))

(defun propcheck-generate-integer ()
  (let* ((bits-in-integers (1+ (log most-positive-fixnum 2)))
         (bytes-needed (ceiling (/ bits-in-integers 8.0)))
         (rand-bytes (propcheck--draw-bytes propcheck--seed bytes-needed))
         (result 0))
    (dolist (byte rand-bytes)
      (setq result
            (+ (* result 256)
               byte)))
    result))

(defun propcheck-generate-ascii-char ()
  "Generate a number that's an ASCII char.
Note that elisp does not have a separate character type."
  ;; between 32 and 126
  (let* ((rand-bytes (propcheck--draw-bytes propcheck--seed 1))
         (byte (car rand-bytes))
         (min-ascii 32)
         (max-ascii 126)
         (ascii-range (- max-ascii min-ascii)))
    (+ min-ascii (mod byte ascii-range))))

(defun propcheck-generate-list (item-generator)
  "Generate a list whose items are drawn from ITEM-GENERATOR."
  (let ((result nil))
    ;; Dumb: 75% chance of making the list bigger on each draw.
    ;; See utils.py/more in Hypothesis for a smarter approach.
    (while (>= (car (propcheck--draw-bytes propcheck--seed 1)) 64)
      (push (funcall item-generator) result))
    result))

(defun propcheck-should (valid-p)
  (unless valid-p
    (throw 'counterexample
           propcheck--seed)))

(defun propcheck--find-counterexample (fun)
  "Call FUN until it finds a counterexample.

Returns the seed that produced the counterexample, or
nil if no counterexamples were found after
`propcheck-max-examples' attempts."
  (let ((seed
         (catch 'counterexample
           (dotimes (_ propcheck-max-examples)
             ;; Generate a fresh seed and try the function.
             (let ((propcheck--seed (propcheck-seed)))
               (funcall fun)))
           nil)))
    (when seed
      (propcheck--seek-start seed))))


(provide 'propcheck)
;;; propcheck.el ends here
