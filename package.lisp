;;;; package.lisp

(defpackage #:cl-riff
  (:use #:cl 
	#:alexandria)
  (:export #:read-chunk
	   #:read-chunks
	   #:read-riff-file
	   #:read-u2
	   #:read-u4))

