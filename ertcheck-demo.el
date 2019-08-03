(require 'ertcheck)

(defun ertcheck-buggy-zerop (num)
  ;; Return t for all values >= 0, so our minimal example should be 1.
  (>= num 0))

(defun wh-zerop ()
  (let* ((i (ertcheck-generate-integer))
         (result (ertcheck-buggy-zerop i)))
    ;; If `zerop' returns t, we should also return t, otherwise we
    ;; should return false.
    (ertcheck--should
     (if (zerop i) result (not result)))))

;; Expecting counterexample: 1.
(let ((td (ertcheck--find-small-counterexample #'wh-zerop))
      (ertcheck--shrinks-remaining t))
  (ertcheck--generate-integer td))

(defun ertcheck-buggy-max-pair (x y)
  (if (< x 101)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

;; Predicates return true if the function did the right thing with the
;; input given.

(defun ertcheck-max-pair-predicate ()
  (let* ((x (ertcheck-generate-integer))
         (y (ertcheck-generate-integer))
         (result (ertcheck-buggy-max-pair x y)))
    ;; For simplicity, only check the case when x is less than y.
    (when (< x y)
      ;; we should have returned y.
      (ertcheck--should (eq result y)))))

;; TODO: Expecting counterexample: 101, 102
(let ((td (ertcheck--find-small-counterexample #'ertcheck-max-pair-predicate))
      (ertcheck--shrinks-remaining t))
  (list (ertcheck--generate-integer td)
        (ertcheck--generate-integer td)))

(defun ertcheck-buggy-max-item (items)
  (car items))

(defun ertcheck-max-items-predicate ()
  (let* ((items (ertcheck-generate-list #'ertcheck-generate-integer))
         (result (ertcheck-buggy-max-item items)))
    (if items
        (eq (car (-sort #'> items))
            result)
      t)))

;; TODO: Expecting counterexample: '(1 2)
(defun ertcheck-demo-max-item ()
  (let ((minimal-testdata (ertcheck-harness #'ertcheck-max-items-predicate)))
    (message "Shrunk example: %S\nitems: %s"
             minimal-testdata
             (ertcheck-generate-list #'ertcheck-generate-integer))))
