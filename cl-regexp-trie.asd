#|
  This file is a part of cl-regexp-trie project.
|#

(defsystem "cl-regexp-trie"
  :version "0.1.0"
  :author "dice-k-nakatani"
  :license "MIT"
  :depends-on (:series
               :cl-ppcre
               :cl-ppcre-unicode
               )
  :components ((:module "src"
                :components
                ((:file "cl-regexp-trie"))))
  :description "port of perl's regexp::trie"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-regexp-trie-test"))))
