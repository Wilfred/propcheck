(require 'propcheck)

(ert-deftest propcheck--draw-bytes--fresh ()
  (let* ((seed (propcheck-seed))
         (bytes (propcheck--draw-bytes seed 10)))
    (should
     (= (length bytes) 10))
    (should
     (= (propcheck-seed-i seed) 10))))

(ert-deftest propcheck--draw-bytes--replay ()
  (let* ((propcheck--allow-replay t)
         (seed (propcheck-seed '(1 2 3 4)))
         (bytes (propcheck--draw-bytes seed 4)))
    (should
     (equal bytes '(1 2 3 4)))
    (should
     (= (propcheck-seed-i seed) 4))))

(ert-deftest propcheck--seek-start ()
  (let* ((propcheck--allow-replay t)
         (seed (propcheck-seed '(1 2 3 4)))
         (bytes (propcheck--draw-bytes seed 4)))
    (should
     (equal bytes '(1 2 3 4)))
    (should
     (= (propcheck-seed-i seed) 4))))

(ert-deftest propcheck-generate-bool ()
  (let* ((propcheck--allow-replay t)
         (propcheck--seed (propcheck-seed '(0))))
    (should
     (null (propcheck-generate-bool))))
  (let* ((propcheck--allow-replay t)
         (propcheck--seed (propcheck-seed '(1))))
    (should
     (propcheck-generate-bool))))

(ert-deftest propcheck-generate-integer ()
  (let* ((propcheck--allow-replay t)
         (propcheck--seed (propcheck-seed '(0 0 0 0 0 0 0 0))))
    (should
     (zerop (propcheck-generate-integer))))
  (let* ((propcheck--allow-replay t)
         (propcheck--seed (propcheck-seed '(0 0 0 0 0 0 1 1))))
    (should
     (= (propcheck-generate-integer) 257))))

(ert-deftest propcheck-generate-ascii-char ()
  (let* ((propcheck--allow-replay t)
         (propcheck--seed (propcheck-seed '(0))))
    (eq (propcheck-generate-ascii-char) ?\ )))

(defun propcheck--buggy-zerop (num)
  ;; Return t for all values >= 0, so our minimal example should be 1.
  (>= num 0))

(defun propcheck--buggy-zerop-test ()
  (let* ((i (propcheck-generate-integer))
         (result (propcheck--buggy-zerop i)))
    ;; If `zerop' returns t, we should also return t, otherwise we
    ;; should return false.
    (propcheck-should
     (if (zerop i) result (not result)))))

(ert-deftest propcheck--find-counterexample ()
  (let* ((found-seed
          (propcheck--find-counterexample #'propcheck--buggy-zerop-test)))
    ;; We should have found a seed for a counterexample.
    (should found-seed)
    ;; That should should have i reset, so we can replay.
    (should (eq (propcheck-seed-i found-seed) 0))
    ;; The input found should make the test fail.
    (let ((propcheck--allow-replay t)
          (propcheck--seed found-seed))
      (should
       (catch 'counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))

(ert-deftest propcheck--find-small-counterexample ()
  (let* ((found-seed
          (propcheck--find-small-counterexample #'propcheck--buggy-zerop-test)))
    ;; We should have found a seed for a counterexample.
    (should found-seed)
    ;; That should should have i reset, so we can replay.
    (should (eq (propcheck-seed-i found-seed) 0))
    ;; The input found should make the test fail.
    (let ((propcheck--allow-replay t)
          (propcheck--seed found-seed))
      (should
       (catch 'counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))

(ert-deftest propcheck--shrink-counterexample ()
  (let* ((seed (propcheck-seed '(255 255 255 255 255 255 255 1)))
         (small-seed
          (propcheck--shrink-counterexample
           #'propcheck--buggy-zerop-test seed 20)))
    ;; The smaller input found should make the test fail.
    (let ((propcheck--allow-replay t)
          (propcheck--seed small-seed))
      (should
       (catch 'counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))

(defun propcheck--zerop-examples ()
  "Generate several counterexamples to see how often we produce
the optimal result."
  (let (examples)
    (dotimes (_ 20)
      (let* ((found-seed
              (propcheck--find-small-counterexample #'propcheck--buggy-zerop-test))
             (propcheck--seed found-seed)
             (propcheck--allow-replay t))
        (push (propcheck-generate-integer) examples)))
    examples))

;; Ideal result: 1 every time.
(propcheck--zerop-examples)

(defun propcheck--buggy-max-pair (x y)
  (if (< x 101)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

(defun propcheck--buggy-max-pair-test ()
  (let* ((x (propcheck-generate-integer))
         (y (propcheck-generate-integer))
         (result (propcheck--buggy-max-pair x y)))
    ;; For simplicity, only check the case when x is less than y.
    (when (< x y)
      ;; we should have returned y.
      (propcheck-should (eq result y)))))

(defun propcheck--max-pair-examples ()
  "Generate several counterexamples to see how often we produce
the optimal result."
  (let (examples)
    (dotimes (_ 10)
      (let* ((found-seed
              (propcheck--find-small-counterexample #'propcheck--buggy-max-pair-test))
             (propcheck--seed found-seed)
             (propcheck--allow-replay t))
        (push
         (list (propcheck-generate-integer)
               (propcheck-generate-integer))
         examples)))
    examples))

;; Ideal result: (101 102) every time.
(propcheck--max-pair-examples)

(defun propcheck--buggy-max-item (items)
  (car items))

(defun propcheck--buggy-max-item-test ()
  (let* ((items (propcheck-generate-list #'propcheck-generate-integer))
         (result (propcheck--buggy-max-item items)))
    (when items
      (propcheck-should (eq (car (-sort #'> items)) result)))))

(defun propcheck--max-item-examples ()
  "Generate several counterexamples to see how often we produce
the optimal result."
  (let (examples)
    (dotimes (_ 10)
      (let* ((found-seed
              (propcheck--find-small-counterexample
               #'propcheck--buggy-max-item-test))
             (propcheck--seed found-seed)
             (propcheck--allow-replay t))
        (push
         (propcheck-generate-list #'propcheck-generate-integer)
         examples)))
    examples))

;; TODO: Expecting counterexample: (0 1)
(propcheck--max-item-examples)
