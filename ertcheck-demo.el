(require 'ertcheck)

(defun ertcheck-buggy-max (x y)
  (if (< x 10)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

(defun ertcheck-demo-predicate (testdata)
  (let* ((x (ertcheck-generate-integer testdata))
         (y (ertcheck-generate-integer testdata))
         (result (ertcheck-buggy-max x y)))
    (and (< x y) (not (eq result y)))))

(defun ertcheck-demo-max ()
  (let ((testdata nil)
        (found-example nil))
    (catch 'found-example
      ;; Search for an example.
      (dotimes (_ ertcheck-max-examples)
        (setq testdata (ertcheck-testdata))
        (setq found-example (ertcheck-demo-predicate testdata))
        (ertcheck-freeze testdata)
        (when found-example
          (throw 'found-example t))))
    (when found-example
      (message "Found example: x: %s y: %s \n %S"
               (ertcheck-generate-integer testdata)
               (ertcheck-generate-integer testdata)
               testdata)
      (ertcheck-freeze testdata)
      (setq testdata
            (ertcheck-shrink testdata #'ertcheck-demo-predicate))
      (ertcheck-freeze testdata)
      (message "Shrunk example: x: %s y: %s \n %S"
               (ertcheck-generate-integer testdata)
               (ertcheck-generate-integer testdata)
               testdata))))
