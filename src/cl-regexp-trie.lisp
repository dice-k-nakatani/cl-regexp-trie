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

(defun make-regexp-trie () (make-hash-table))

(defun regexp-trie-add (obj string)
  (declare #.*standard-optimize-settings*)
  (let ((term-obj (collect-fn
                   t
                   (lambda () obj)
                   (lambda (xobj char)
                     (declare #.*standard-optimize-settings*)
                     (let ((xxobj (gethash char xobj)))
                       (unless xxobj
                         (setq xxobj (make-hash-table :size 8))
                         (setf (gethash char xobj) xxobj))
                       xxobj))
                   (scan 'string string))))
    (setf (gethash :term term-obj) t)))

(defun regexp-trie-string (obj)
  (declare #.*standard-optimize-settings*)
  (labels ((walk (xobj)
             (declare #.*standard-optimize-settings*)
             (declare (type hash-table xobj))
             (let (alt
                   cc
                   termp
                   groupedp ;; already grouped. so do not need wrap with (?: )
                   result)
               ;;(when (and (gethash :term xobj) (= 1 (collect-length (scan-hash xobj))))
               (when (and (gethash :term xobj) (not (collect-nth 1 (scan-hash xobj))))
                 (return-from walk))
               ;; sort hash key to stabilize the result.
               (iterate ((char (scan (sort (collect (scan-hash xobj))
                                           (lambda (a b)
                                             (cond
                                               ((eq :term a) nil)
                                               ((eq :term b) t)
                                               (t (char<= a b))))))))
                 (let ((xxobj (gethash char xobj))
                       (qchar (quote-meta-chars (format nil "~a" char))))
                   (if (hash-table-p xxobj)
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
