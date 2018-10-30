(require 'ertcheck)

(defun ertcheck-buggy-max (x y)
  (if (< x 10)
      ;; Correct implementation.
      (if (< x y) x y)
    ;; Here's a bug!
    11))

(defun ertcheck-demo-max ()
  (let ((testdata nil)
        (found-example nil))
    (catch 'found-example
      ;; Search for an example.
      (dotimes (_ ertcheck-max-examples)
        (setq testdata (ertcheck-testdata))
        (let* ((x (ertcheck-generate-integer testdata))
               (y (ertcheck-generate-integer testdata))
               (result (ertcheck-buggy-max x y)))
          (when (and (< x y) (not (eq result y)))
            (ertcheck-freeze testdata)
            (setq found-example t)
            (throw 'found-example t)))))
    (when found-example
      (message "Found example: %S" testdata))))
