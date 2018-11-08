(require 'ertcheck)

(ert-deftest ertcheck-generate-integer ()
  (let ((testdata (ertcheck-testdata
                   (make-list 8 0) 0 nil t)))
    (should
     (zerop (ertcheck-generate-integer testdata)))))
