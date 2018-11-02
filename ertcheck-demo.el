(require 'ertcheck)

(defun ertcheck-buggy-max-pair (x y)
  (if (< x 101)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

(defun ertcheck-buggy-max-item (items)
  (car items))

;; Predicates return true on counter examples.

(defun ertcheck-max-pair-predicate (testdata)
  (let* ((x (ertcheck-generate-integer testdata))
         (y (ertcheck-generate-integer testdata))
         (result (ertcheck-buggy-max-pair x y)))
    (and (< x y) (not (eq result y)))))

(defun ertcheck-max-items-predicate (testdata)
  (let* ((items (ertcheck-generate-list testdata #'ertcheck-generate-integer))
         (result (ertcheck-buggy-max-item items)))
    (not (eq (car (-sort #'> items))
             result))))

(defun ertcheck-harness (predicate)
  (let ((testdata nil)
        (found-example nil))
    (catch 'found-example
      ;; Search for an example.
      (dotimes (_ ertcheck-max-examples)
        (setq testdata (ertcheck-testdata))
        (setq found-example (funcall predicate testdata))
        (ertcheck-freeze testdata)
        (when found-example
          (throw 'found-example t))))
    (when found-example
      (message "Found initial example: %S" testdata)
      (ertcheck-freeze testdata)
      (setq testdata
            (ertcheck-shrink testdata predicate))
      (ertcheck-freeze testdata))
    testdata))

(defun ertcheck-demo-max-pair ()
  (let ((testdata (ertcheck-harness #'ertcheck-max-pair-predicate)))
    (message "Shrunk example: %S\nx: %s y: %s"
             testdata
             (ertcheck-generate-integer testdata)
             (ertcheck-generate-integer testdata))))

(defun ertcheck-demo-max-item ()
  (let ((testdata (ertcheck-harness #'ertcheck-max-items-predicate)))
    (message "Shrunk example: %S\nitems: %s"
             testdata
             (ertcheck-generate-list testdata #'ertcheck-generate-integer))))
