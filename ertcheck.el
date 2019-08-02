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
(require 'ert)
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

(defvar ertcheck--testdata
  nil
  "The current testdata instance being used to generate values.")

(defun ertcheck--draw-bytes (testdata num-bytes)
  "Get NUM-BYTES of random data (generating if necessary), write
it to TESTDATA and return it."
  (ertcheck--assert-td)
  (let* ((i (ertcheck-testdata-i testdata))
         (new-i (+ i num-bytes)))
    ;; Update i to record our position in the bytes.
    (setf (ertcheck-testdata-i testdata) new-i)
    ;; Store which regions of data were accessed, as ranges.
    (push (cons i new-i) (ertcheck-testdata-blocks testdata))

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

(defun ertcheck--freeze (testdata)
  "Return a frozen copy of TESTDATA, which we can use to shrink."
  (when (ertcheck-testdata-frozen testdata)
    (error "data already frozen"))
  (let ((i (ertcheck-testdata-i testdata))
        (bytes (ertcheck-testdata-bytes testdata))
        (blocks (ertcheck-testdata-blocks testdata)))
    (ertcheck-testdata (-take i bytes) 0 blocks t)))

(defun ertcheck--seek-start (testdata)
  "Move i to the start of TESTDATA."
  (unless (ertcheck-testdata-frozen testdata)
    (error "data should be frozen"))
  (let ((bytes (ertcheck-testdata-bytes testdata))
        (blocks (ertcheck-testdata-blocks testdata)))
    (ertcheck-testdata bytes 0 blocks t)))

(defun ertcheck--set-byte (testdata i value)
  "Return a copy of TESTDATA with byte I set to VALUE."
  (let ((bytes (ertcheck-testdata-bytes testdata)))
    (ertcheck-testdata
     (-replace-at i value bytes)
     (ertcheck-testdata-i testdata)
     (ertcheck-testdata-blocks testdata)
     (ertcheck-testdata-frozen testdata))))

(defun ertcheck-generate-bool ()
  (ertcheck--generate-bool ertcheck--testdata))

(defun ertcheck--generate-bool (testdata)
  (let ((rand-byte (car (ertcheck--draw-bytes testdata 1))))
    (not (zerop (logand rand-byte 1)))))

(defun ertcheck-generate-integer ()
  (ertcheck--generate-integer ertcheck--testdata))

(defun ertcheck--generate-integer (testdata)
  (let* ((bits-in-integers (1+ (log most-positive-fixnum 2)))
         (bytes-needed (ceiling (/ bits-in-integers 8.0)))
         (rand-bytes (ertcheck--draw-bytes testdata bytes-needed))
         (result 0))
    (dolist (byte rand-bytes)
      (setq result
            (+ (* result 256)
               byte)))
    result))

(defun ertcheck-generate-ascii-char ()
  "Generate a number that's an ASCII char.
Note that elisp does not have a separate character type."
  (ertcheck--generate-ascii-char ertcheck--testdata))

(defun ertcheck--generate-ascii-char (testdata)
  "Generate a number that's an ASCII char.
Note that elisp does not have a separate character type."
  ;; between 32 and 126
  (let* ((rand-bytes (ertcheck--draw-bytes testdata 1))
         (byte (car rand-bytes))
         (min-ascii 32)
         (max-ascii 126)
         (ascii-range (- max-ascii min-ascii)))
    (+ min-ascii (mod byte ascii-range))))

;; TODO: circular lists, improprer lists/trees.
(defun ertcheck-generate-list (item-generator)
  "Generate a list whose items are drawn from ITEM-GENERATOR."
  (ertcheck--generate-list ertcheck--testdata item-generator))

(defun ertcheck--generate-list (testdata item-generator)
  "Generate a list whose items are drawn from ITEM-GENERATOR."
  (let ((result nil))
    ;; Dumb: 75% chance of making the list bigger on each draw.
    ;; See utils.py/more in Hypothesis for a smarter approach.
    (while (>= (car (ertcheck--draw-bytes testdata 1)) 64)
      (push (funcall item-generator) result))
    result))

(defun ertcheck-generate-vector (item-generator)
  "Generate a vector whose items are drawn from ITEM-GENERATOR."
  (ertcheck--generate-vector ertcheck--testdata item-generator))

(defun ertcheck--generate-vector (testdata item-generator)
  "Generate a vector whose items are drawn from ITEM-GENERATOR."
  (apply #'vector
         (ertcheck--generate-list testdata item-generator)))

(defun ertcheck-generate-string ()
  "Generate an ASCII string."
  (ertcheck--generate-string ertcheck--testdata))

(defun ertcheck--generate-string (testdata)
  "Generate a string using TESTDATA."
  (let ((chars nil))
    ;; Dumb: 75% chance of making the string bigger on each draw.
    ;; TODO: see what hypothesis does
    ;; TODO: multibyte support, key sequence support
    (while (>= (car (ertcheck--draw-bytes testdata 1)) 64)
      (push
       (ertcheck--generate-ascii-char testdata)
       chars))
    (concat chars)))

(defun ertcheck--should (valid-p)
  (unless valid-p
    (throw 'counterexample
           ertcheck--testdata)))

(defun ertcheck--find-counterexample (fun)
  "Call FUN until it finds a counterexample.

Returns the frozen testdata that produced the counterexample, or
nil if no counterexamples were found after
`ertcheck-max-examples' attempts."
  (let ((td
         (catch 'counterexample
           (dotimes (_ ertcheck-max-examples)
             ;; Generate a fresh testdata and try the function.
             (let ((ertcheck--testdata (ertcheck-testdata)))
               (funcall fun)))
           nil)))
    (when td
      (ertcheck--freeze td))))

(defvar ertcheck--shrinks-remaining nil)

(defun ertcheck--assert-td ()
  (unless ertcheck--testdata
    (error "ertcheck--testdata must not be nil"))
  (if ertcheck--shrinks-remaining
      (progn
        (unless (ertcheck-testdata-frozen ertcheck--testdata)
          (error "Data should be frozen during shrinking"))
        (unless (<
                 (ertcheck-testdata-i ertcheck--testdata)
                 (length (ertcheck-testdata-bytes ertcheck--testdata)))
          (error "Data should be replayed during shrinking")))
    (progn
      (when (ertcheck-testdata-frozen ertcheck--testdata)
        (error "Data should not be frozen when finding counterexamples"))
      (unless (=
               (ertcheck-testdata-i ertcheck--testdata)
               (length (ertcheck-testdata-bytes ertcheck--testdata)))
        (error "Data should be growing when finding counterexamples")))))

(defun ertcheck--shrink-by (test-fn shrink-fn testdata)
  "Attempt to shrink TESTDATA by calling TEST-FN with smaller values.
Reduce the size of TESTDATA by applying SHRINK-FN."
  (let ((changed t))
    ;; Keep going until we run out of shrinks, or we stop finding
    ;; testdata values with more zeroes.
    (catch 'out-of-shrinks
      (while changed
        (setq changed nil)

        (dotimes (i (length (ertcheck-testdata-bytes testdata)))
          (let* ((shrunk-testdata
                  (funcall shrink-fn testdata i)))
            (when shrunk-testdata
              (let* ((ertcheck--testdata shrunk-testdata)
                     (new-testdata
                      (catch 'counterexample
                        (funcall test-fn)
                        nil)))
                (when new-testdata
                  (setq changed t)
                  (setq testdata
                        (ertcheck--seek-start new-testdata))))

              (cl-decf ertcheck--shrinks-remaining)
              (unless (> ertcheck--shrinks-remaining 0)
                (throw 'out-of-shrinks t))))))))
  testdata)

(defun ertcheck--zero-byte (testdata i)
  "Set byte at I in TESTDATA to zero if it isn't already."
  (let* ((bytes (ertcheck-testdata-bytes testdata))
         (byte (nth i bytes)))
    (unless (zerop byte)
      (ertcheck--set-byte testdata i 0))))

(defun ertcheck--halve-byte (testdata i)
  "Divide byte at I in TESTDATA by 2 if it will produce
a different testdata."
  (let* ((bytes (ertcheck-testdata-bytes testdata))
         (byte (nth i bytes)))
    (unless (or (eq byte 0) (eq byte 1))
      (ertcheck--set-byte testdata i (/ byte 2)))))

(defun ertcheck--decrement-byte (testdata i)
  "Divide byte at I in TESTDATA by 2 if it will produce
a different testdata."
  (let* ((bytes (ertcheck-testdata-bytes testdata))
         (byte (nth i bytes)))
    ;; TODO: should we increment the previous byte too?
    (unless (zerop byte)
      (ertcheck--set-byte testdata i (1- byte)))))

(defun ertcheck--shrink-counterexample (fun td shrinks)
  "Call FUN up to SHRINKS times, to find a smaller TD that still
fails."
  (let* ((ertcheck--shrinks-remaining shrinks)
         )
    (->> td
         (ertcheck--shrink-by fun #'ertcheck--zero-byte)
         (ertcheck--shrink-by fun #'ertcheck--halve-byte)
         (ertcheck--shrink-by fun #'ertcheck--decrement-byte))))

(defun ertcheck--find-small-counterexample (fun)
  (let ((td
         (ertcheck--find-counterexample fun)))
    (when td
      (ertcheck--shrink-counterexample fun td ertcheck-max-shrinks))))

(provide 'ertcheck)
;;; ertcheck.el ends here
