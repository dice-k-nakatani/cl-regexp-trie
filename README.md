# cl-regexp-trie

## synopsis

```
$ ros install dice-k-nakatani/cl-regexp-trie
$ ros run
* (require :cl-regexp-trie)
* (defpackage :scratch (:use :cl :cl-regexp-trie))
* (in-package :scratch)
* (defparameter rt (make-regexp-trie))
* (dolist (word (list "foobar" "fooxar" "foozap" "fooza")) (regexp-trie-add rt word))
* (format t "~a~%" (regexp-trie-string rt)) ;; output: foo(?:bar|xar|zap?)
* (ppcre:regex-replace-all (regexp-trie-scanner rt) "foobar fooxar foozap fooza survivor" "killed") ;; replaced
```

## desription

fast keyword matching with the trie algorithm (a common lisp port of Perl's Regexp::Trie).

## see also

* https://metacpan.org/pod/Regexp::Trie

* https://github.com/gfx/ruby-regexp_trie


## license

this is available under the terms of the [MIT License](http://opensource.org/licenses/MIT).

the original code is [Regexp::Trie](https://metacpan.org/pod/Regexp::Trie).
