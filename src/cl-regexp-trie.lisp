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
    (ppcre:regex-replace-all non-word-char-scanner string "\\\\\\&"
                             :start start :end end)))

;; ------------------------------

(defun make-node () (list nil))

(defmacro node-val (node key)
  `(getf (car ,node) ,key))

(defmacro node-set (node key val)
  `(setf (getf (car ,node) ,key) ,val))

(defun node-p (node)
  (and (listp node)
       (listp (car node))))

(defmacro scan-node (node)
  `(scan-plist (car ,node)))

;; ------------------------------
(defun make-regexp-trie () (make-node))

(defun regexp-trie-add (obj string)
  (declare #.*standard-optimize-settings*)
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
  (declare #.*standard-optimize-settings*)
  (labels ((walk (xobj)
             (declare #.*standard-optimize-settings*)
             (let (alt
                   cc
                   termp
                   groupedp ;; already grouped. so do not need wrap with (?: )
                   result)
               (when (and (node-val xobj :term) (not (collect-nth 1 (scan-node xobj))))
                 (return-from walk))
               ;; sort node key to stabilize the result.
               (iterate ((char (scan (sort (collect (scan-node xobj))
                                           (lambda (a b)
                                             (cond
                                               ((eq :term a) nil)
                                               ((eq :term b) t)
                                               (t (char<= a b))))))))
                 (let ((xxobj (node-val xobj char))
                       (qchar (quote-meta-chars (format nil "~a" char))))
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
                           (with-output-to-string (*standard-output*)
                             (princ "[")
                             (dolist (c (nreverse cc))
                               (princ c))
                             (princ "]"))
                           )
                       alt))
               (setq result
                     (if (= 1 (length alt))
                         (format nil "~a" (car alt))
                         (with-output-to-string (stream)
                           (princ "(?:" stream)
                           ;;(format stream "~{~a~^|~}" (nreverse alt))
                           (iterate (((curr next) (chunk 2 1 (scan (nreverse (cons nil alt))))))
                             (if next
                                 (progn
                                   (princ curr stream)
                                   (princ "|" stream))
                                 (princ curr stream)))
                           (princ ")" stream)
                           )))
               (setq groupedp (or groupedp
                                  (< 1 (length alt))))
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
