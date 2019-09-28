(require 'propcheck)

(ert-deftest propcheck--draw-bytes--fresh ()
  (let* ((seed (propcheck-seed))
         (bytes (propcheck--draw-bytes seed 10)))
    (should
     (= (length bytes) 10))
    (should
     (= (propcheck-seed-i seed) 10))
    (should
     (equal (propcheck-seed-intervals seed)
            '((0 10))))))

(ert-deftest propcheck--draw-bytes--replay ()
  (let* ((propcheck--replay t)
         (seed (propcheck-seed '(1 2 3 4)))
         (bytes (propcheck--draw-bytes seed 4)))
    (should
     (equal bytes '(1 2 3 4)))
    (should
     (= (propcheck-seed-i seed) 4))
    (should
     (equal (propcheck-seed-intervals seed)
            '((0 4))))))

(ert-deftest propcheck--seek-start ()
  (let ((seed
         (propcheck--seek-start
          (propcheck-seed '(1 2 3 4 5) 4 '((0 4))))))
    ;; We should have set i back to 0.
    (should
     (equal (propcheck-seed-i seed) 0))
    ;; Unused bytes should be truncated.
    (should
     (equal (propcheck-seed-bytes seed)
            '(1 2 3 4)))))

(ert-deftest propcheck--set-byte ()
  (let* ((seed (propcheck-seed '(1 2 3 4) 4 '((0 4))))
         (result (propcheck--set-byte seed 1 0)))
    (should
     (equal
      (propcheck-seed-bytes result)
      '(1 0 3 4)))
    (should
     (equal (propcheck-seed-i result) 4))
    (should
     (equal (propcheck-seed-intervals result) '((0 4))))))

(ert-deftest propcheck-generate-bool ()
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(0))))
    (should
     (null (propcheck-generate-bool nil))))
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(255))))
    (should
     (propcheck-generate-bool nil))))

(ert-deftest propcheck-generate-one-of ()
  "We should generate one item from the list of choices."
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(0)))
         (choices (list "a" "b" "c")))
    (should
     (member (propcheck-generate-one-of nil :values choices) choices))))

(ert-deftest propcheck-generate-one-of--power-2 ()
  "For lists whose length is a power of two, later items should
be chosen with higher seeds."
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(128)))
         (choices (list "a" "b" "c" "d")))
    (should
     (equal (propcheck-generate-one-of nil :values choices) "c"))))

(ert-deftest propcheck-generate-integer--zero ()
  "Ensure a zero byte gives us a zero output, so we shrink
towards zero."
  (let* ((propcheck--replay t)
         (propcheck-seed
          (propcheck-seed '(0 0 0 0 0 0 0 0))))
    (should
     (zerop (propcheck-generate-integer nil)))))

(ert-deftest propcheck-generate-integer--max ()
  "Ensure the max seed returns the max number (considered as an
unsigned integer)."
  (let* ((propcheck--replay t)
         (propcheck-seed
          (propcheck-seed '(255 255 255 255 255 255 255 255 255))))
    (should
     (= (propcheck-generate-integer nil) -1))))

(ert-deftest propcheck-generate-integer--range-min ()
  "When specifying a range, zero seeds should give MIN."
  (let* ((propcheck--replay t)
         (propcheck-seed
          (propcheck-seed '(0 0 0 0 0 0 0 0))))
    (should
     (= (propcheck-generate-integer nil :min 5) 5))))

(ert-deftest propcheck-generate-integer--range-max ()
  "If the range is a power of 2, the max seed should give us MAX."
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(255))))
    (should
     (= (propcheck-generate-integer nil :min 0 :max 255) 255))))

(ert-deftest propcheck-generate-proper-list ()
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(255 255 0)))
         (list (propcheck-generate-proper-list nil :value-fn #'propcheck-generate-bool)))
    (should
     (equal list '(t)))))

(ert-deftest propcheck-generate-vector ()
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(255 255 0)))
         (vec (propcheck-generate-vector nil :value-fn #'propcheck-generate-bool)))
    (should
     (equal vec [t]))))

(ert-deftest propcheck-generate-string ()
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(64 1 0)))
         (str (propcheck-generate-string nil)))
    (should
     (equal str "!"))))

(ert-deftest propcheck-generate-ascii-char ()
  (let* ((propcheck--replay t)
         (propcheck-seed (propcheck-seed '(0))))
    (eq (propcheck-generate-ascii-char nil) ?\ )))

(defun propcheck--buggy-zerop (num)
  ;; Return t for all values >= 0, so our minimal example should be 1.
  (>= num 0))

(defun propcheck--buggy-zerop-test ()
  (let* ((i (propcheck-generate-integer "i"))
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
    (let ((propcheck--replay t)
          (propcheck-seed found-seed))
      (should
       (catch 'propcheck--counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))

(defun propcheck--buggy-1+ (x)
  (when (> x 23)
    (error "yikes!"))
  (1+ x))

(defun propcheck--buggy-1+-test ()
  (let* ((i (propcheck-generate-integer "i")))
    (propcheck-should
     (= (propcheck--buggy-1+ i) (1+ i)))))

(ert-deftest propcheck--find-counterexample--errors ()
  "Ensure we handle sample functions that throw errors."
  (let* ((found-seed
          (propcheck--find-counterexample #'propcheck--buggy-1+-test)))
    ;; We should have found a seed for a counterexample.
    (should found-seed)
    ;; The input found should make the test error.
    (let ((propcheck--replay t)
          (propcheck-seed found-seed))
      (condition-case nil
          (progn
            (funcall #'propcheck--buggy-1+-test)
            ;; We should have errored and not reached this.
            (should nil))
        (error nil)))))

(ert-deftest propcheck--find-small-counterexample ()
  (let* ((found-seed
          (propcheck--find-small-counterexample #'propcheck--buggy-zerop-test)))
    ;; We should have found a seed for a counterexample.
    (should found-seed)
    ;; That should should have i reset, so we can replay.
    (should (eq (propcheck-seed-i found-seed) 0))
    ;; The input found should make the test fail.
    (let ((propcheck--replay t)
          (propcheck-seed found-seed))
      (should
       (catch 'propcheck--counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))

(ert-deftest propcheck--shrink-counterexample ()
  (let* ((seed (propcheck-seed '(255 255 255 255 255 255 255 1)))
         (small-seed
          (propcheck--shrink-counterexample
           #'propcheck--buggy-zerop-test seed 20)))
    ;; The smaller input found should make the test fail.
    (let ((propcheck--replay t)
          (propcheck-seed small-seed))
      (should
       (catch 'propcheck--counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))

(ert-deftest propcheck--shrink-counterexample--overrun ()
  "Ensure we handle overruns gracefully."
  ;; This seed is too small: it's only one byte, but
  ;; `propcheck--buggy-zerop-test' requires more bytes to generate an
  ;; integer, so we overrun.
  (let ((seed (propcheck-seed '(0) 1 '((0 1)))))
    ;; No assertion, just ensure we don't error.
    (propcheck--shrink-counterexample #'propcheck--buggy-zerop-test seed 20)))

(ert-deftest propcheck--zero-interval ()
  (let* ((seed (propcheck-seed '(0 1 2 3) 0 '((0 2))))
         (result (propcheck--zero-interval seed 0)))
    (should
     (equal
      (propcheck-seed-bytes result)
      '(0 0 2 3)))))

(ert-deftest propcheck--zero-interval--already-zero ()
  (let ((seed (propcheck-seed '(0 0 2 3) 0 '((0 2)))))
    (should
     (null (propcheck--zero-interval seed 0)))))

(ert-deftest propcheck--subtract-interval ()
  (let* ((seed (propcheck-seed '(10 20) 0 '((0 2))))
         (result (propcheck--subtract-interval seed 0 1)))
    (should
     (equal
      (propcheck-seed-bytes result)
      '(10 19)))))

(ert-deftest propcheck--subtract-interval--carry ()
  (let* ((seed (propcheck-seed '(1 0 0) 0 '((0 3))))
         (result (propcheck--subtract-interval seed 0 1)))
    (should
     (equal
      (propcheck-seed-bytes result)
      '(0 255 255)))))

(ert-deftest propcheck--subtract-interval--too-small ()
  (let ((seed (propcheck-seed '(0 0 5) 0 '((0 3)))))
    (should
     (null (propcheck--subtract-interval seed 0 10)))))

(ert-deftest propcheck--shift-right-interval ()
  (let* ((seed (propcheck-seed '(0 4 2 1 0) 0 '((0 5))))
         (result (propcheck--shift-right-interval seed 0 1)))
    (should
     (equal
      (propcheck-seed-bytes result)
      '(0 2 1 0 128)))))

(ert-deftest propcheck--shift-right-interval--zeroes ()
  "Return nil if shifting bytes will give the same result"
  (let* ((seed (propcheck-seed '(0 0 0 0 0) 0 '((0 5)))))
    (should
     (null (propcheck--shift-right-interval seed 0 1)))))

(ert-deftest propcheck--list-< ()
  (should
   (propcheck--list-< '(1 2) '(3 4)))
  (should
   (propcheck--list-< '(1 2) '(1 4)))
  (should
   (not (propcheck--list-< '(2 2) '(2 2))))
  (should
   (not (propcheck--list-< '(2 4) '(2 3)))))

(ert-deftest propcheck--swap-intervals ()
  (let* ((seed (propcheck-seed '(5 4 3 2 1) 0 '((3 5) (2 3) (0 2))))
         (result (propcheck--swap-intervals seed 0 2)))
    (should
     (equal (propcheck-seed-bytes result) '(2 1 3 5 4))))
  ;; Don't try to swap intervals of different lengths.
  (let ((seed (propcheck-seed '(1 2 3 4 5) 0 '((1 5) (0 4)))))
    (should
     (null (propcheck--swap-intervals seed 0 1)))))

(ert-deftest propcheck-shrinking-buggy-zerop ()
  "Ensure that we find the minimal counterexample for
`propcheck--buggy-zerop'."
  (let ((optimal-count 0))
    (dotimes (_ 10)
      (let* ((found-seed
              (propcheck--find-small-counterexample #'propcheck--buggy-zerop-test))
             (propcheck-seed found-seed)
             (propcheck--replay t)
             (num (propcheck-generate-integer nil)))
        (when (= num 1)
          (cl-incf optimal-count))))
    ;; We should get the optimal result at least 90% of the time.
    (should (>= optimal-count 9))))

(defun propcheck--buggy-max-pair (x y)
  (if (< x 101)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

(defun propcheck--buggy-max-pair-test ()
  (let* ((x (propcheck-generate-integer "x"))
         (y (propcheck-generate-integer "y"))
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
             (propcheck-seed found-seed)
             (propcheck--replay t))
        (push
         (list (propcheck-generate-integer "x")
               (propcheck-generate-integer "y"))
         examples)))
    examples))

(ert-deftest propcheck-shrinking-buggy-zerop ()
  "Ensure that we find the minimal counterexample for
`propcheck--buggy-zerop'."
  (let ((optimal-count 0))
    (dotimes (_ 10)
      (let* ((found-seed
              (propcheck--find-small-counterexample #'propcheck--buggy-zerop-test))
             (propcheck-seed found-seed)
             (propcheck--replay t)
             (i (propcheck-generate-integer nil)))
        (when (= i 1)
          (cl-incf optimal-count))))
    ;; We should get the optimal result at least 90% of the time.
    (should (>= optimal-count 9))))

(defun propcheck--buggy-max-item (items)
  (car items))

(defun propcheck--buggy-max-item-test ()
  (let* ((items (propcheck-generate-proper-list nil :value-fn #'propcheck-generate-integer))
         (result (propcheck--buggy-max-item items)))
    (when items
      (propcheck-should (eq (car (-sort #'> items)) result)))))

(defun propcheck--max-item-examples ()
  "Generate several counterexamples to see how often we produce
the optimal result."
  (let (examples)
    (dotimes (_ 10)
      (let* ((found-seed
              (propcheck--find-small-counterexample #'propcheck--buggy-max-item-test))
             (propcheck-seed found-seed)
             (propcheck--replay t))
        (push
         (propcheck-generate-proper-list nil :value-fn #'propcheck-generate-integer)
         examples)))
    examples))

;; Ideal result: (0 1) or (-1 0)
(propcheck--max-item-examples)
