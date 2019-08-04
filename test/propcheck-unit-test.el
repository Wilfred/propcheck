(require 'propcheck)

(ert-deftest propcheck--draw-bytes--fresh ()
  (let* ((seed (propcheck-seed))
         (bytes (propcheck--draw-bytes seed 10)))
    (should
     (= (length bytes) 10))
    (should
     (= (propcheck-seed-i seed) 10))))

(ert-deftest propcheck--draw-bytes--replay ()
  (let* ((propcheck--shrinks-remaining 999)
         (seed (propcheck-seed '(1 2 3 4)))
         (bytes (propcheck--draw-bytes seed 4)))
    (should
     (equal bytes '(1 2 3 4)))
    (should
     (= (propcheck-seed-i seed) 4))))

(ert-deftest propcheck--seek-start ()
  (let* ((propcheck--shrinks-remaining 999)
         (seed (propcheck-seed '(1 2 3 4)))
         (bytes (propcheck--draw-bytes seed 4)))
    (should
     (equal bytes '(1 2 3 4)))
    (should
     (= (propcheck-seed-i seed) 4))))

(ert-deftest propcheck-generate-bool ()
  (let* ((propcheck--shrinks-remaining 999)
         (propcheck--seed (propcheck-seed '(0))))
    (should
     (null (propcheck-generate-bool))))
  (let* ((propcheck--shrinks-remaining 999)
         (propcheck--seed (propcheck-seed '(1))))
    (should
     (propcheck-generate-bool))))

(ert-deftest propcheck-generate-integer ()
  (let* ((propcheck--shrinks-remaining 999)
         (propcheck--seed (propcheck-seed '(0 0 0 0 0 0 0 0))))
    (should
     (zerop (propcheck-generate-integer))))
  (let* ((propcheck--shrinks-remaining 999)
         (propcheck--seed (propcheck-seed '(0 0 0 0 0 0 1 1))))
    (should
     (= (propcheck-generate-integer) 257))))

(ert-deftest propcheck-generate-ascii-char ()
  (let* ((propcheck--shrinks-remaining 999)
         (propcheck--seed (propcheck-seed '(0))))
    (eq (propcheck-generate-ascii-char) ?\ )))
