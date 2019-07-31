(require 'ertcheck)

(defun ertcheck-buggy-zerop (num)
  ;; Return t for all values >= 0, so our minimal example should be 1.
  (>= num 0))

(defun wh-zerop ()
  (let* ((i (ertcheck-generate-integer))
         (result (ertcheck-buggy-zerop i)))
    (message "i: %s result: %s zerop: %s should: %s"
             i result (zerop i)
             (if (zerop i) result (not result)))
    ;; If `zerop' returns t, we should also return t, otherwise we
    ;; should return false.
    (ertcheck--should
     (if (zerop i) result (not result)))))

(let ((td (ertcheck--find-small-counterexample #'wh-zerop)))
  (ertcheck--generate-integer td))

(ertcheck--generate-integer ertcheck--testdata)

(defun ertcheck-zerop-predicate ()
  (let* ((i (ertcheck-generate-integer))
         (result (ertcheck-buggy-zerop i)))
    ;; If `zerop' returns t, we should also return t, otherwise we
    ;; should return false.
    (if (zerop i) result (not result))))

;; Expecting counterexample: 1.
(defun ertcheck-demo-zerop ()
  (let ((minimal-testdata (ertcheck-harness #'ertcheck-zerop-predicate)))
    (message "Shrunk example: %S\ni: %s"
             minimal-testdata
             (ertcheck--generate-integer minimal-testdata))))

(defun ertcheck-buggy-max-pair (x y)
  (if (< x 101)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

(defun ertcheck-buggy-max-item (items)
  (car items))

;; Predicates return true if the function did the right thing with the
;; input given.

(defun ertcheck-max-pair-predicate ()
  (let* ((x (ertcheck-generate-integer))
         (y (ertcheck-generate-integer))
         (result (ertcheck-buggy-max-pair x y)))
    ;; If x was less than y,
    (if (< x y)
        ;; we should have returned y.
        (eq result y)
      ;; Don't check the case when x is greater than y.
      t)))

(defun ertcheck-max-items-predicate ()
  (let* ((items (ertcheck-generate-list #'ertcheck-generate-integer))
         (result (ertcheck-buggy-max-item items)))
    (if items
        (eq (car (-sort #'> items))
            result)
      t)))

(defun ertcheck-harness (valid-p)
  "Call VALID-P repeatedly, and return a small testdata where
VALID-P returns nil.

Returns nil if VALID-P passes all `ertcheck-max-examples'
attempts."
  (let ((ertcheck--testdata nil)
        (found-example nil))
    (catch 'found-example
      ;; Search for an example.
      (dotimes (_ ertcheck-max-examples)
        (setq ertcheck--testdata (ertcheck-testdata))
        (setq found-example (not (funcall valid-p)))
        (setq ertcheck--testdata (ertcheck--freeze ertcheck--testdata))
        (when found-example
          (throw 'found-example t))))
    (when found-example
      (message "Found initial example: %S" ertcheck--testdata)
      (setq ertcheck--testdata
            (ertcheck--shrink ertcheck--testdata valid-p)))
    ertcheck--testdata))

;; TODO: Expecting counterexample: 101, 102
(defun ertcheck-demo-max-pair ()
  (let ((minimal-testdata (ertcheck-harness #'ertcheck-max-pair-predicate)))
    (message "Shrunk example: %S\nx: %s y: %s"
             minimal-testdata
             (ertcheck--generate-integer minimal-testdata)
             (ertcheck--generate-integer minimal-testdata))))

;; TODO: Expecting counterexample: '(1 2)
(defun ertcheck-demo-max-item ()
  (let ((minimal-testdata (ertcheck-harness #'ertcheck-max-items-predicate)))
    (message "Shrunk example: %S\nitems: %s"
             minimal-testdata
             (ertcheck-generate-list #'ertcheck-generate-integer))))
