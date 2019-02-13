# cl-regexp-trie

## synopsis

```
$ ros install dice-k-nakatani/cl-regexp-trie
$ ros run
* (require :cl-regexp-trie)
* (defpackage :scratch (:use :cl :cl-regexp-trie))
* (in-package :scratch)
* (defparameter rt (make-regexp-trie))
* (dolist (word (list "foobar" "fooxar" "foozap" "fooza"))
    (regexp-trie-add rt word))
* (format t "~a~%" (regexp-trie-string rt))
;; output: foo(?:bar|xar|zap?)

* (ppcre:regex-replace-all (regexp-trie-scanner rt) 
    "foobar fooxar foozap fooza survivor" "killed")
;; replaced
```

## desription

fast keyword matching with the trie algorithm (a common lisp port of Perl's Regexp::Trie).

cl-regexp-trie takes an arbitrary number of strings and assembles them into a single optimized regular expression.

## benchmark

`./example/benchmark.ros` compares 10000 words ppcre's replace speed.

scanner-trie is trie optimized scanner. scanner-alt is simple alternation scanner (like "word-a|word-b|word-c").

```
;; result:
;;
;; CPU Model:	Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz
;; Total real memory available:	2013 MB
;; Operating system release:	FreeBSD 11

---- replacing with scanner-trie
Evaluation took:
  0.154 seconds of real time
  0.097264 seconds of total run time (0.097264 user, 0.000000 system)
  62.99% CPU
  353,067,691 processor cycles
  250,992 bytes consed
  
---- replacing with scanner-alt
Evaluation took:
  2.133 seconds of real time
  1.343570 seconds of total run time (1.343570 user, 0.000000 system)
  63.01% CPU
  4,894,064,445 processor cycles
  243,136 bytes consed
```

## see also

* https://metacpan.org/pod/Regexp::Trie

* https://github.com/gfx/ruby-regexp_trie


## license

this is available under the terms of the [MIT License](http://opensource.org/licenses/MIT).

the original code is [Regexp::Trie](https://metacpan.org/pod/Regexp::Trie).
