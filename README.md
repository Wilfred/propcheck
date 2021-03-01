# propcheck [![Coverage Status](https://coveralls.io/repos/github/Wilfred/propcheck/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/propcheck?branch=master)

propcheck brings property based testing to elisp.

It's similar to the excellent [Hypothesis](https://hypothesis.works/)
library for Python.

Status: beta. It works, but expect rough edges.

## Usage

Tests are defined with `propcheck-deftest`, which defines an ERT test
just like `ert-deftest`.

Your test should generate input values with the propcheck generator
functions, then call `propcheck-should` to make assertions about the
result.

```emacs-lisp
(require 'propcheck)

(defun buggy-zerop (num)
  ;; Return t for all values >= 0. This is wrong! We'll claim that 42
  ;; is zero.
  ;;
  ;; There are lots of numbers that produce a wrong result from this
  ;; function, but 1 is the smallest. Ideally propcheck would report 1
  ;; as failing example. It usually does.
  (>= num 0))

(propcheck-deftest buggy-zerop-should-match-zerop ()
  ;; The body of this test will be evaluated repeatedly (up to
  ;; `propcheck-max-examples` times). The value generated will be
  ;; different on each iteration.
  ;;
  ;; If the assertion ever fails, propcheck will call the body again
  ;; with progressively smaller values, then report the smallest
  ;; failing example it could find.
  (let* ((i (propcheck-generate-integer "i")))
    (propcheck-should
     (eq (zerop i)
         (buggy-zerop i)))))
```

## Generators

propcheck provides the following generators:

* `propcheck-generate-bool`
* `propcheck-generate-integer`
* `propcheck-generate-float`
* `propcheck-generate-ascii-char`
* `propcheck-generate-proper-list`
* `propcheck-generate-vector`
* `propcheck-generate-string`
* `propcheck-generate-one-of`

### Using Generators Interactively

Generally you'll want to use `propcheck-deftest` to handle seeds for
you. You can still experiement with generator functions in `M-x ielm`
if you bind `propcheck-seed` first. Here's an example:

``` emacs-lisp
(let ((propcheck-seed (propcheck-seed)))
  (propcheck-generate-string nil)) ; e.g. "M26gM{^*v "
```

