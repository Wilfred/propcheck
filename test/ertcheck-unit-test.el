(require 'ertcheck)

(ert-deftest ertcheck-generate-integer ()
  (let ((testdata (ertcheck-testdata
                   (make-list 8 0) 0 nil t)))
    (should
     (zerop (ertcheck-generate-integer testdata)))))

(ert-deftest ertcheck-shrink ()
  (let* ((testdata (ertcheck-testdata
                    '(255) 0 nil t))
         (predicate (lambda (testdata)
                      ;; We need to draw a byte or shrinking will give
                      ;; us an empty testdata.
                      (ertcheck-draw-bytes testdata 1)
                      t))
         (shrunk (ertcheck-shrink testdata predicate)))
    (should
     (equal (ertcheck-testdata-bytes shrunk)
            '(0)))))

(ert-deftest ertcheck-draw-bytes ()
  "Ensure that we set blocks correctly when drawing bytes."
  (let ((testdata (ertcheck-testdata
                   (make-list 8 0) 0 nil t)))
    (ertcheck-draw-bytes testdata 2)
    (should
     (equal (ertcheck-testdata-blocks testdata)
            '((0 . 2))))))
