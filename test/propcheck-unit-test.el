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
    ;; We should generate this integer again.
    (let ((propcheck--allow-replay t)
          (propcheck--seed found-seed))
      (should
       (catch 'counterexample
         (funcall #'propcheck--buggy-zerop-test)
         nil)))))
