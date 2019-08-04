(require 'ertcheck)


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
