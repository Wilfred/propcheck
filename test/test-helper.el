;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'f)

(let ((ertcheck-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path ertcheck-dir))

(require 'undercover)
(undercover "ertcheck.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
