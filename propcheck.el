;;; propcheck.el --- quickcheck/hypothesis style testing for elisp         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wilfred Hughes
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.11.0") (dash-functional "1.2.0"))

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
(require 'dash-functional)
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
                    (i 0)
                    (groups nil))))
  bytes
  i
  groups)

(defun propcheck--draw-bytes (seed num-bytes)
  "Get NUM-BYTES of random data from SEED, generating if necessary.
This function modifies SEED and returns the generated bytes."
  (propcheck--sanity-check seed)
  (let* ((i (propcheck-seed-i seed))
         (bytes (propcheck-seed-bytes seed))
         (new-i (+ i num-bytes)))
    ;; Update i to record our position in the bytes.
    (setf (propcheck-seed-i seed) new-i)

    ;; Record the groups of bytes generated.
    (push (list i new-i) (propcheck-seed-groups seed))

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

(defun propcheck--debug (&rest _)
  "Debugging helper function"
  ;; Deliberately don't do anything with arguments. Instead, use M-x
  ;; trace-function to see values passed to this function when also
  ;; tracing shrinking logic.
  )

(defun propcheck--seek-start (seed)
  "Return a copy of SEED with i set to the beginning."
  (let ((i (propcheck-seed-i seed))
        (bytes (propcheck-seed-bytes seed))
        (groups (propcheck-seed-groups seed)))
    (propcheck--debug "remaining" propcheck--shrinks-remaining)
    (propcheck-seed
     (-take i bytes)
     0
     groups)))

(defun propcheck--no-groups (seed)
  "Return a copy of SEED with no groups"
  (let ((i (propcheck-seed-i seed))
        (bytes (propcheck-seed-bytes seed)))
    (propcheck-seed
     bytes
     i
     nil)))

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
  (let ((sign (car (propcheck--draw-bytes propcheck--seed 1))))
    ;; 50% chance of negative numbers.
    (if (<= sign 128)
        (propcheck--generate-positive-integer)
      (1-
       (- (propcheck--generate-positive-integer))))))

(defun propcheck--generate-positive-integer ()
  (let* ((bits-in-integers (round (log most-positive-fixnum 2)))
         (bytes-needed (ceiling (/ bits-in-integers 8.0)))
         (high-bits-needed (- bits-in-integers
                              (* (1- bytes-needed) 8)))
         (rand-bytes (propcheck--draw-bytes propcheck--seed bytes-needed))
         (result 0))
    (--each-indexed rand-bytes
      ;; Avoid overflow for the bits in the highest byte.
      (when (zerop it-index)
        (setq it (lsh it (- high-bits-needed 8))))

      (setq result
            (+ (* result 256)
               it)))
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

;; TODO: circular lists, improprer lists/trees.
(defun propcheck-generate-proper-list (item-generator)
  "Generate a list whose items are drawn from ITEM-GENERATOR."
  (let ((result nil))
    ;; Make the list bigger most of the time. 50 is the threshold used
    ;; in ListStrategy.java in hypotheseis-java.
    ;; See utils.py/more in Hypothesis for a smarter approach.
    (while (> (car (propcheck--draw-bytes propcheck--seed 1)) 50)
      (push (funcall item-generator) result))
    ;; Reverse the list so the order in the seed reflects the order in
    ;; the list. This can produce nicer looking shrunk values.
    (nreverse result)))

(defun propcheck-generate-vector (item-generator)
  "Generate a vector whose items are drawn from ITEM-GENERATOR."
  (apply #'vector
         (propcheck-generate-proper-list item-generator)))

(defun propcheck-generate-string ()
  "Generate a string."
  (let ((chars nil))
    ;; Dumb: 75% chance of making the string bigger on each draw.
    ;; TODO: see what hypothesis does
    ;; TODO: multibyte support, key sequence support
    (while (>= (car (propcheck--draw-bytes propcheck--seed 1)) 64)
      (push
       (propcheck-generate-ascii-char)
       chars))
    (concat chars)))

(defun propcheck-should (valid-p)
  (propcheck--debug "valid" valid-p)
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

(defun propcheck--shrink-group-by (test-fn shrink-fn seed)
  "Attempt to shrink SEED by calling TEST-FN with smaller values.
Reduce the size of SEED by applying SHRINK-FN."
  (let ((i 0)
        (changed t))
    ;; Keep going until we run out of shrinks, or we stop finding
    ;; bytes that we can shrink.
    (catch 'out-of-shrinks
      (while changed
        (setq changed nil)
        (setq i 0)

        ;; The seed might shrink during iteration, so keep checking
        ;; the length.
        (while (< i (length (propcheck-seed-groups seed)))
          (let* ((shrunk-seed (funcall shrink-fn seed i)))
            (when shrunk-seed
              (propcheck--debug
               (list "shrinking group" i
                     (nth i (reverse (propcheck-seed-groups seed)))))
              (let* ((propcheck--seed
                      ;; Discard the group data before calling
                      ;; TEST-FN, so we can see if it uses different
                      ;; groups in this run.
                      (propcheck--no-groups shrunk-seed))
                     (new-seed
                      (catch 'counterexample
                        (funcall test-fn)
                        nil)))
                (when new-seed
                  (setq changed t)
                  (setq seed
                        (propcheck--seek-start new-seed))))

              (cl-decf propcheck--shrinks-remaining)
              (unless (> propcheck--shrinks-remaining 0)
                (throw 'out-of-shrinks t))))

          (cl-incf i)))))
  seed)

(defun propcheck--shrink-byte-by (test-fn shrink-fn seed)
  "Attempt to shrink SEED by calling TEST-FN with smaller values.
Reduce the size of SEED by applying SHRINK-FN."
  (let ((i 0)
        (changed t))
    ;; Keep going until we run out of shrinks, or we stop finding
    ;; bytes that we can shrink.
    (catch 'out-of-shrinks
      (while changed
        (setq changed nil)
        (setq i 0)

        ;; The seed might shrink during iteration, so keep checking
        ;; the length.
        (while (< i (length (propcheck-seed-bytes seed)))
          (let* ((shrunk-seed (funcall shrink-fn seed i)))
            (when shrunk-seed
              (let* ((propcheck--seed shrunk-seed)
                     (new-seed
                      (catch 'counterexample
                        (funcall test-fn)
                        nil)))
                (when new-seed
                  (setq changed t)
                  (setq seed
                        (propcheck--seek-start new-seed))))

              (cl-decf propcheck--shrinks-remaining)
              (unless (> propcheck--shrinks-remaining 0)
                (throw 'out-of-shrinks t))))

          (cl-incf i)))))
  seed)

(defun propcheck--set-byte (seed i value)
  "Return a copy of SEED with byte I set to VALUE."
  (let ((bytes (propcheck-seed-bytes seed))
        (groups (propcheck-seed-groups seed)))
    (propcheck-seed
     (-replace-at i value bytes)
     (propcheck-seed-i seed)
     groups)))

(defun propcheck--zero-byte (seed i)
  "Set byte at I in SEED to zero if it isn't already."
  (let* ((bytes (propcheck-seed-bytes seed))
         (byte (nth i bytes)))
    (unless (zerop byte)
      (propcheck--set-byte seed i 0))))

(defun propcheck--subtract-byte (seed i amount)
  "subtract AMOUNT at I in SEED."
  (let* ((bytes (propcheck-seed-bytes seed))
         (byte (nth i bytes)))
    (when (> byte amount)
      (propcheck--set-byte seed i (- byte amount)))))

(defun propcheck--zero-group (seed n)
  "Set group N in SEED to zero. Returns a copy of SEED."
  (-let* ((seed-bytes (propcheck-seed-bytes seed))
          ((group-start group-end)
           (nth n (reverse (propcheck-seed-groups seed))))
          (group-bytes
           (-slice seed-bytes group-start group-end)))
    (unless (-all-p #'zerop group-bytes)
      (dotimes (i (- group-end group-start))
        (setq seed
              (propcheck--set-byte seed (+ group-start i) 0)))
      seed)))

(defun propcheck--shift-right-group (seed n amount)
  "Shift right by AMOUNT in group N in SEED.
Returns a copy of SEED.
Assumes AMOUNT is not greater than 8."
  (-let* ((seed-bytes (propcheck-seed-bytes seed))
          ((group-start group-end)
           (nth n (reverse (propcheck-seed-groups seed))))
          (group-bytes
           (-slice seed-bytes group-start group-end))
          (new-bytes nil)
          (carry 0))
    (dolist (byte group-bytes)
      ;; Given byte 0xN, we build 2 byte number 0xN0,
      ;; shift to produce 0xPQ, then P is our new byte and Q is
      ;; the carry.
      (let* ((padded-byte (lsh byte 8)))
        ;; Do the shift.
        (setq padded-byte (lsh padded-byte (- amount)))
        ;; Extract the new byte and new carry.
        (setq byte (+ (logand (lsh padded-byte -8) 255)
                      carry))
        (setq carry (logand padded-byte 255))
        (push byte new-bytes)))

    (-each-indexed (nreverse new-bytes)
      (lambda (i byte)
        (setq seed
              (propcheck--set-byte seed (+ group-start i) byte))))
    seed))

(defun propcheck--shrink-counterexample (fun seed shrinks)
  "Call FUN up to SHRINKS times, to find a smaller version of SEED that still
fails."
  (let* ((propcheck--shrinks-remaining shrinks))
    (->> seed
         (propcheck--shrink-group-by fun #'propcheck--zero-group)
         (propcheck--shrink-byte-by fun #'propcheck--zero-byte)
         (propcheck--shrink-group-by fun (-rpartial #'propcheck--shift-right-group 1))
         (propcheck--shrink-byte-by fun (-rpartial #'propcheck--subtract-byte 10))
         (propcheck--shrink-byte-by fun (-rpartial #'propcheck--subtract-byte 1)))))

(defun propcheck--find-small-counterexample (fun)
  (let ((seed
         (propcheck--find-counterexample fun)))
    (when seed
      (propcheck--shrink-counterexample fun seed propcheck-max-shrinks))))

(provide 'propcheck)
;;; propcheck.el ends here
