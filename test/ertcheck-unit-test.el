(require 'ertcheck)

(ert-deftest ertcheck--generate-integer ()
  (let ((testdata (ertcheck-testdata
                   (make-list 8 0) 0 nil t)))
    (should
     (zerop (ertcheck--generate-integer testdata)))))

(ert-deftest ertcheck--generate-list ()
  (let* ((testdata (ertcheck-testdata
                    (make-list 8 0) 0 nil t))
         (nums
          (ertcheck--generate-list testdata #'ertcheck-generate-integer)))
    (dolist (num nums)
      (should
       (integerp num)))))

(ert-deftest ertcheck--shrink ()
  (let* ((ertcheck--testdata (ertcheck-testdata
                              '(255) 0 nil t))
         (predicate (lambda ()
                      ;; We need to draw a byte or shrinking will give
                      ;; us an empty testdata.
                      (ertcheck--draw-bytes ertcheck--testdata 1)
                      t))
         (shrunk (ertcheck--shrink ertcheck--testdata predicate)))
    (should
     (equal (ertcheck-testdata-bytes shrunk)
            '(0)))))

(ert-deftest ertcheck--draw-bytes ()
  "Ensure that we set blocks correctly when drawing bytes."
  (let ((ertcheck--testdata (ertcheck-testdata
                             (make-list 8 0) 0 nil t)))
    (ertcheck--draw-bytes ertcheck--testdata 2)
    (should
     (equal (ertcheck-testdata-blocks ertcheck--testdata)
            '((0 . 2))))))
