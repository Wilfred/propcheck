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
