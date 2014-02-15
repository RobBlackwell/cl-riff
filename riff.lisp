;;;; riff.lisp

(in-package #:cl-riff)

(defun read-fourcc (in)
  "Reads a four character tag (FOURCC) and returns it as an Ascii string. Returns NIL if end of file."
  (let ((b (read-byte in nil)))
    (when b
      (coerce
       (list
	(code-char b)
	(code-char (read-byte in))
	(code-char (read-byte in))
	(code-char (read-byte in))) 'string))))

(defun read-u4 (in)
  "Reads a 4 byte little-endian integer."
  (let ((u4 0))
    (setf (ldb (byte 8 0) u4) (read-byte in))
    (setf (ldb (byte 8 8) u4) (read-byte in))
    (setf (ldb (byte 8 16) u4) (read-byte in))
    (setf (ldb (byte 8 24) u4) (read-byte in))
    u4))

(defun read-u2 (in)
  "Reads a 2 byte little-endian integer."
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    u2))

(defun read-chunk (in)
  "Reads a chunk and returns it as a plist or NIL if end of file."
  (let ((chunk-id (read-fourcc in)))
    (when chunk-id
      (let
	  ((chunk-data-size (read-u4 in)))

	(if (or (string= chunk-id "RIFF") (string= chunk-id "LIST"))
	    (list :chunk-id chunk-id :file-type (read-fourcc in))
	    (let*
		((chunk-data (make-array chunk-data-size :element-type (stream-element-type in))))
	      (read-sequence chunk-data in)
	      (when (oddp chunk-data-size)
		;; Discard pad character.
		(read-byte in))
	      (list :chunk-id chunk-id :chunk-data chunk-data)))))))

(defun read-chunks (in)
  "Reads all the chunks until end of file. Returns a list of chunks."
  (loop for x = (read-chunk in)
       while x
       collect x))

(defun read-riff-file (filename)
  "Reads a RIFF format file returning a list of chunks."
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (read-chunks stream)))



