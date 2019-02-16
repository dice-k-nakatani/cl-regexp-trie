(defpackage cl-regexp-trie
  (:use
   :cl
   :series)
  (:export
   :make-regexp-trie
   :regexp-trie-add
   :regexp-trie-string
   :regexp-trie-scanner))
(in-package :cl-regexp-trie)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *standard-optimize-settings*
    '(optimize
      (speed 3)
      (space 1)
      (debug 1)
      (safety 1)
      (compilation-speed 1))
    "The standard optimize settings used by most declaration expressions.")
  )

;; FIXME: this is naive hack not to quote full width char.
;; modified version of ppcre's quote-meta-chars
(let* ((ppcre::*use-bmh-matchers* nil)
       (non-word-char-scanner (ppcre:create-scanner "[^a-zA-Z_0-9\\p{Hiragana}\\p{Katakana}\\p{Han}]")))
  (defun quote-meta-chars (string &key (start 0) (end (length string)))
    (declare #.*standard-optimize-settings*)
    (ppcre:regex-replace-all non-word-char-scanner string "\\\\\\&"
                             :start start :end end)))

(defvar *non-word-char-scanner* (ppcre:create-scanner "[^a-zA-Z_0-9\\p{Hiragana}\\p{Katakana}\\p{Han}]"))
(defmacro need-quote-meta-char-p (string)
  `(ppcre:scan *non-word-char-scanner* ,string))

;; ------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (remove :regexp-trie-plist *features*)
  )

#+regexp-trie-plist
(progn
  #|
`plist` surprised me. like:
(defvar a)
(defvar b)
(setq a '(:a 1 :b 2))
(setq b a)
(assert (eq a b))

(setf (getf a :b) 20)
(assert (eq a b))

(setf (getf a :c) 30)
(assert (not (eq a b))) ;; ??

I can not use plist as hash-table.
I must wrap it with cons or somrthing.
  |#

  (defmacro make-node () `(list nil))
  (defmacro node-val (node key)
    `(getf (car ,node) ,key))
  (defmacro node-set (node key val)
    `(setf (getf (car ,node) ,key) ,val))
  (defmacro node-p (node)
    `(and (listp ,node)
          (listp (car ,node))))
  (defmacro scan-node (node)
    `(scan-plist (car ,node)))
  )
#-regexp-trie-plist
(progn
  (defmacro make-node ()
    `(make-hash-table :test 'eq))
  (defmacro node-val (node key)
    `(gethash ,key ,node))
  (defmacro node-set (node key val)
    `(setf (gethash ,key ,node) ,val))
  (defmacro node-p (node)
    `(hash-table-p ,node))
  (defmacro scan-node (node)
    `(scan-hash ,node))
  )


;; ------------------------------
(defun make-regexp-trie () (make-node))

(defun regexp-trie-add (obj string)
  (let ((term-obj (collect-fn
                   t
                   (lambda () obj)
                   (lambda (xobj char)
                     (declare #.*standard-optimize-settings*)
                     (let ((xxobj (node-val xobj char)))
                       (unless xxobj
                         (setq xxobj (make-node))
                         (node-set xobj char xxobj))
                       xxobj))
                   (scan 'string string))))
    (node-set term-obj :term t)))

(defun regexp-trie-string (obj)
  (labels ((walk (xobj)
             (let (alt
                   cc
                   termp
                   groupedp ;; already grouped. so do not need wrap with (?: )
                   result)
               (when (and (node-val xobj :term) (not (collect-nth 1 (scan-node xobj))))
                 (return-from walk))
               (iterate ((char
                          (scan (sort (collect (scan-node xobj))
                                      (lambda (a b)
                                        (cond
                                          ((eq :term a) nil)
                                          ((eq :term b) t)
                                          (t (char<= a b))))))
                          ))
                 (let ((xxobj (node-val xobj char))
                       (qchar (let ((str (princ-to-string char)))
                                (if (need-quote-meta-char-p str)
                                    (format nil "\\~a" char)
                                    str))))
                   (if (node-p xxobj)
                       (let ((recurse (walk xxobj)))
                         (if recurse
                             (push (format nil "~a~a" qchar recurse) alt)
                             (push qchar cc)))
                       (setf termp t)))
                 )

               (setq groupedp (not alt))

               (when cc
                 (push (if (= 1 (length cc))
                           (car cc)
                           (format nil "[~{~a~}]" (nreverse cc)))
                       alt))

               (setq groupedp (or groupedp (< 1 (length alt))))

               (setq result
                     (if (= 1 (length alt))
                         (format nil "~a" (car alt))
                         (format nil "(?:~{~a~^|~})" (nreverse alt))))

               (when termp
                 (setq result (if groupedp
                                  (format nil "~a?" result)
                                  (format nil "(?:~a)?" result))))
               result)))
    (walk obj)
    ))

(defun regexp-trie-scanner (obj
                            &rest keys
                            &key case-insensitive-mode
                              multi-line-mode
                              single-line-mode
                              extended-mode
                              destructive)
  (declare #.*standard-optimize-settings*)
  (let ((string (regexp-trie-string obj)))
    (apply #'ppcre:create-scanner string keys)))
