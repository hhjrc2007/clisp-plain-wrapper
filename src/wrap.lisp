;; Text is read and written as ISO-8859-1 so that every byte passes
;; through unchanged whatever the locale is; words are only decoded as
;; UTF-8 to measure them.
(defparameter *bytes* (ext:make-encoding :charset charset:iso-8859-1
    :line-terminator :unix))

(defparameter *utf-8* (ext:make-encoding :charset charset:utf-8
    :input-error-action :error))

(defun whitespacep (char)
    (member char '(#\Space #\Tab)))

(defun decode (string)
    (handler-case
        (ext:convert-string-from-bytes
            (ext:convert-string-to-bytes string *bytes*) *utf-8*)
        (error () string)))

(defun text-width (string)
    (let ((column 0))
    (loop for char across (decode string)
        do (if (char= char #\Tab)
            (setf column (* 8 (1+ (floor column 8))))
            (incf column (ext:char-width char))))
    column))

(defun split-words (line)
    (loop with end = 0
        for start = (position-if-not #'whitespacep line :start end)
        while start
        do (setf end (or (position-if #'whitespacep line :start start)
            (length line)))
        collect (subseq line start end)))

(defun wrap-line (line width)
    (let* ((indent (subseq line 0
            (or (position-if-not #'whitespacep line) 0)))
        (width (- width (text-width indent)))
        (lines '())
        (current nil)
        (current-width 0))
    (dolist (word (split-words line))
        (let ((word-width (text-width word)))
        (cond ((null current)
            (setf current word
                current-width word-width))
            ((<= (+ current-width 1 word-width) width)
            (setf current (concatenate 'string current " " word)
                current-width (+ current-width 1 word-width)))
            (t
            (push (concatenate 'string indent current) lines)
            (setf current word
                current-width word-width)))))
    (push (if current (concatenate 'string indent current) "") lines)
    (nreverse lines)))

(defun wrap-file (path width)
    (with-open-file (in path :external-format *bytes*)
        (loop for line = (read-line in nil)
            while line
            do (dolist (wrapped (wrap-line line width))
                (write-line wrapped)))))

(defun fail (control &rest args)
    (format *error-output* "wrap.lisp: ~?~%" control args)
    (ext:exit 1))

(defun parse-width (string)
    (let ((width (parse-integer string :junk-allowed t)))
    (unless (and width (plusp width)
        (every #'digit-char-p (string-trim " " string)))
        (fail "invalid width ~s" string))
    width))

(let ((path (first ext:*args*))
      (width (second ext:*args*)))
  (unless (and path (null (cddr ext:*args*)))
      (format *error-output* "usage: clisp src/wrap.lisp <file> [width]~%")
      (ext:exit 1))
  (setf width (if width (parse-width width) 72))
  (setf (stream-external-format *standard-output*) *bytes*)
  (handler-case (wrap-file path width)
      (file-error () (fail "cannot read ~a" path))))
