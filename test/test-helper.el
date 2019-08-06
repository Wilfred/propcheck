;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'f)

(let ((propcheck-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path propcheck-dir))

(require 'undercover)
(undercover "propcheck.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
