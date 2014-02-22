;;;; package.lisp

(defpackage #:riff
  (:use #:cl 
	#:alexandria)
  (:export #:read-riff-chunk
	   #:read-riff-chunks
	   #:read-riff-file
	   #:find-riff-chunk
	   #:read-u2
	   #:read-u4
	   #:default-chunk-data-reader
	   #:riff-chunk-id
	   #:riff-chunk-data
	   #:riff-chunk-data-size
	   #:riff-file-type))

