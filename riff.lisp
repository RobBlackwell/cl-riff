;;;; riff.lisp

(in-package #:riff)

(defun read-fourcc (stream)
  "Reads a four character tag (fourcc) from STREAM and returns it as
an ascii string. Returns nil at end of file."
  (let ((byte (read-byte stream nil)))
    (when byte
      (coerce
       (list
	(code-char byte)
	(code-char (read-byte stream))
	(code-char (read-byte stream))
	(code-char (read-byte stream))) 'string))))

(defun read-fourcc* (stream)
  "Reads a four character tag (fourcc) from STREAM and returns it as
a reversed ascii string. Returns nil at end of file.
Used with xfir format."
  (let ((4cc (read-fourcc stream)))
    (when 4cc
      (nreverse 4cc))))

(defun read-u4 (stream)
  "Reads a 4 byte little-endian integer from STREAM."
  (let ((u4 0))
    (setf (ldb (byte 8 0) u4) (read-byte stream))
    (setf (ldb (byte 8 8) u4) (read-byte stream))
    (setf (ldb (byte 8 16) u4) (read-byte stream))
    (setf (ldb (byte 8 24) u4) (read-byte stream))
    u4))

(defun read-u4* (stream)
  "Reads a 4 byte big-endian integer from STREAM.
Used with rifx and xfir formats."
  (let ((u4 0))
    (setf (ldb (byte 8 24) u4) (read-byte stream))
    (setf (ldb (byte 8 16) u4) (read-byte stream))
    (setf (ldb (byte 8 8) u4) (read-byte stream))
    (setf (ldb (byte 8 0) u4) (read-byte stream))
    u4))

(defun read-u2 (stream)
  "Reads a 2 byte little-endian integer from STREAM."
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    (setf (ldb (byte 8 8) u2) (read-byte stream))
    u2))

(defun read-u2* (stream)
  "Reads a 2 byte big-endian integer from STREAM."
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte stream))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    u2))

(defun default-chunk-data-reader (stream chunk-id chunk-data-size)
  "Reads chunk-data from STREAM as an array of CHUNK-DATA-SIZE bytes.
Second argument CHUNK-ID is ignored."
  (declare (ignore chunk-id))
  (let ((chunk-data (make-array chunk-data-size
			       :element-type (stream-element-type stream))))
    (read-sequence chunk-data stream)
    chunk-data))

(defun read-riff-chunk (stream &key (chunk-data-reader #'default-chunk-data-reader) (format :riff))
  "Read a riff file chunk from STREAM and return it as a
plist or nil if end of file.  Optional keyword FORMAT
nil to accept \"RIFF\", \"RIFX\" or \"XFIR\"
as the chunk id and process accordingly; otherwise
:riff (default), :rifx or :xfir to specify the format.
Chunks and subchunks are returned indiscriminately -
discerning subchunk structure is up to the caller.
First returned value is a plist keyed by :chunk-id,
:chunk-data-size and either :file-type or :chunk-data.
Second returned value is the actual format as a keyword."
  (let* ((raw-chunk-id (read-fourcc stream))
	 (magic-cookie-p (member raw-chunk-id '("RIFF" "RIFX" "XFIR")
				 :test #'string=))
	 (chunk-id (if (eq :xfir format)
		       (reverse raw-chunk-id)
		       raw-chunk-id)))
    (when chunk-id
      (case format
	((:riff :rifx :xfir)
	 (when (and magic-cookie-p
		    (string/= raw-chunk-id (symbol-name format)))
	   (error "Got ~A but expected ~A"
		  raw-chunk-id (symbol-name format))))
	((nil)
	 (setq format (cond ((string= chunk-id "RIFF") :riff)
			    ((string= chunk-id "RIFX") :rifx)
			    ((string= chunk-id "XFIR") :xfir)
			    (t (error "Got ~A but expected RIFF, RIFX or XFIR."
				      chunk-id)))))
	(t (error "Unknown format ~A"
		  format)))
      (let* ((chunk-data-size (if (eq :riff format)
				  (read-u4 stream)
				  (read-u4* stream)))
	     (plist (if (or magic-cookie-p
			    (member chunk-id '("LIST")
				    :test #'string=))
			(list :chunk-id chunk-id
			      :chunk-data-size chunk-data-size
			      :file-type (if (eq :xfir format)
					     (read-fourcc* stream)
					     (read-fourcc stream)))
			(let ((chunk-data (funcall chunk-data-reader stream chunk-id chunk-data-size)))
			  (when (oddp chunk-data-size)
			    ;; Discard pad character.
			    (read-byte stream))
			  (list :chunk-id chunk-id
				:chunk-data-size chunk-data-size
				:chunk-data chunk-data)))))
	(values plist format)))))

(defun read-riff-chunks (stream &key (chunk-data-reader #'default-chunk-data-reader) format)
  "Reads all the chunks from STREAM until end of file.
Returns a list of chunks.  Optional keyword FORMAT
defaults to nil to detect the format from the stream,
otherwise :riff, :rifx or :xfir to specify the format."
  (multiple-value-bind (chunk0 format) (read-riff-chunk stream
							:chunk-data-reader chunk-data-reader
							:format format)
    (loop for chunk = chunk0 then (read-riff-chunk stream
						   :chunk-data-reader chunk-data-reader
						   :format format)
	  while chunk
	  collect chunk)))

(defun find-riff-chunk (stream chunk-id &key (chunk-data-reader #'default-chunk-data-reader) format)
  "Reads chunks from stream until a chunk with chunk-id is found,
or nil meaning not found.  Optional keyword FORMAT
defaults to nil to detect the format from the file,
otherwise :riff, :rifx or :xfir to specify the format."
  (multiple-value-bind (chunk0 format) (read-riff-chunk stream
							:chunk-data-reader chunk-data-reader
							:format format)
    (loop for chunk = chunk0 then (read-riff-chunk stream
						   :chunk-data-reader chunk-data-reader
						   :format format)
	  while chunk
	  until (string= (riff-chunk-id chunk) chunk-id)
	  finally (return chunk))))

(defun read-riff-file (filespec &key (chunk-data-reader #'default-chunk-data-reader) format)
  "Reads a riff format file named by FILESPEC, returning a list of chunks.
Optional keyword FORMAT defaults to nil to detect the format from the file,
otherwise :riff, :rifx or :xfir to specify the format."
  (with-open-file (stream filespec :direction :input :element-type '(unsigned-byte 8))
    (read-riff-chunks stream
		      :chunk-data-reader chunk-data-reader
		      :format format)))

;;; Define an interface for chunks:

(defgeneric riff-chunk-id (chunk) (:documentation "Returns the chunk-id of a riff CHUNK - a four character ascii tag."))
(defgeneric riff-chunk-data-size (chunk) (:documentation "Returns the size of the riff CHUNK data."))
(defgeneric riff-chunk-data (chunk) (:documentation "Returns a byte array being the data in the riff CHUNK."))
(defgeneric riff-file-type (chunk) (:documentation "Returns a four character riff file type - e.g. \"WAVE\""))

;;; Define an implementation based on plists.

(defmethod riff-chunk-id ((chunk cons))
  (getf chunk :chunk-id))

(defmethod riff-chunk-data-size ((chunk cons))
  (getf chunk :chunk-data-size))

(defmethod riff-chunk-data ((chunk cons))
  (getf chunk :chunk-data))

(defmethod riff-chunk-data-start ((chunk cons))
  (getf chunk :chunk-data-start 0))

(defmethod riff-chunk-data-end (chunk)
  (+ (riff-chunk-data-start chunk) (riff-chunk-data-size chunk)))

(defmethod riff-file-type ((chunk cons))
  (getf chunk :file-type))

;;; END
