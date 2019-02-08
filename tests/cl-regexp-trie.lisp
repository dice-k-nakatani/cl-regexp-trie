(defpackage cl-regexp-trie-test
  (:use
   :cl
   :cl-regexp-trie
   :series
   :trivia
   :prove))
(in-package :cl-regexp-trie-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-regexp-trie)' in your Lisp.

(plan nil)

(defun optimized-string (&rest strings)
  (let ((rt (make-regexp-trie)))
    (dolist (string strings)
      (regexp-trie-add rt string))
    (regexp-trie-string rt)))

(subtest "basic"
  (is (optimized-string "a") "a")
  (is (optimized-string "a" "aa") "aa?")
  (is (optimized-string "a" "b") "[ab]")
  (is (optimized-string "foo" "bar") "(?:bar|foo)")
  (is (optimized-string "foo" "bar" "baz") "(?:ba[rz]|foo)")
  (is (optimized-string "foo" "baxx" "bayy") "(?:ba(?:xx|yy)|foo)")
  )

(subtest "groupedp"
  (is (optimized-string "aa" "aax" "aay") "aa[xy]?")
  (is (optimized-string "aa" "aaxx" "aayy") "aa(?:xx|yy)?")
  (is (optimized-string "aa" "aax") "aax?")
  (is (optimized-string "aa" "aaxx") "aa(?:xx)?")
  )

;; ppcre:quote-meta-chars do not work for multiple bytes string.
;; so currently i use cl-regexp-trie::quote-meta-chars. but it is naive hack.
(subtest "japanese"
  (is (optimized-string "あ") "あ")
  (is (optimized-string "あ" "ああ") "ああ?")
  (is (optimized-string "あ" "い") "[あい]")
  (is (optimized-string "ふおお" "いある") "(?:いある|ふおお)")
  (is (optimized-string "ふおお" "いある" "いあず") "(?:いあ[ずる]|ふおお)")
  (is (optimized-string "ふおお" "いあxx" "いあわわ") "(?:いあ(?:xx|わわ)|ふおお)")
  )

(finalize)
