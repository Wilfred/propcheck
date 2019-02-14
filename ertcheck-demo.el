(require 'ertcheck)

(defun ertcheck-buggy-zerop (num)
  ;; Return t for all values >= 0!
  (if (< num 0)
      nil
    t))

(defun ertcheck-zerop-predicate ()
  (let* ((i (ertcheck-generate-integer))
         (result (ertcheck-buggy-zerop i)))
    ;; If `zerop' returns t, we should also return t, otherwise we
    ;; should return false.
    (if (zerop i) result (not result))))

(defun ertcheck-demo-zerop ()
  (let ((minimal-testdata (ertcheck-harness #'ertcheck-zerop-predicate)))
    (message "Shrunk example: %S\ni: %s"
             minimal-testdata
             (ertcheck--generate-integer minimal-testdata))))

(defun ertcheck-buggy-max-pair (x y)
  (if (< x 101)
      ;; Correct implementation.
      (if (< x y) y x)
    ;; Here's a bug!
    11))

(defun ertcheck-buggy-max-item (items)
  (car items))

;; Predicates return true on counter examples.

(defun ertcheck-max-pair-predicate ()
  (let* ((x (ertcheck-generate-integer))
         (y (ertcheck-generate-integer))
         (result (ertcheck-buggy-max-pair x y)))
    (and (< x y) (not (eq result y)))))

(defun ertcheck-max-items-predicate ()
  (let* ((items (ertcheck-generate-list #'ertcheck-generate-integer))
         (result (ertcheck-buggy-max-item items)))
    (not (eq (car (-sort #'> items))
             result))))

(defun ertcheck-harness (predicate)
  (let ((ertcheck--testdata nil)
        (found-example nil))
    (catch 'found-example
      ;; Search for an example.
      (dotimes (_ ertcheck-max-examples)
        (setq ertcheck--testdata (ertcheck-testdata))
        (setq found-example (funcall predicate))
        (setq ertcheck--testdata (ertcheck--freeze ertcheck--testdata))
        (when found-example
          (throw 'found-example t))))
    (when found-example
      (message "Found initial example: %S" ertcheck--testdata)
      (setq ertcheck--testdata
            (ertcheck--shrink ertcheck--testdata predicate)))
    ertcheck--testdata))

(defun ertcheck-demo-max-pair ()
  (let ((testdata (ertcheck-harness #'ertcheck-max-pair-predicate)))
    (message "Shrunk example: %S\nx: %s y: %s"
             testdata
             (ertcheck-generate-integer)
             (ertcheck-generate-integer))))

(defun ertcheck-demo-max-item ()
  (let ((testdata (ertcheck-harness #'ertcheck-max-items-predicate)))
    (message "Shrunk example: %S\nitems: %s"
             testdata
             (ertcheck-generate-list #'ertcheck-generate-integer))))
