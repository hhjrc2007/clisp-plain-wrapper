(defun whitespacep (char)
    (member char '(#\Space #\Tab)))

(defun split-words (line)
    (loop with end = 0
        for start = (position-if-not #'whitespacep line :start end)
        while start
        do (setf end (or (position-if #'whitespacep line :start start)
            (length line)))
        collect (subseq line start end)))

(defun wrap-line (line width)
    (let ((lines '())
        (current nil))
    (dolist (word (split-words line))
        (cond ((null current)
            (setf current word))
            ((<= (+ (length current) 1 (length word)) width)
            (setf current (concatenate 'string current " " word)))
            (t
            (push current lines)
            (setf current word))))
    (push (or current "") lines)
    (nreverse lines)))

(defun wrap-file (path width)
    (with-open-file (in path)
        (loop for line = (read-line in nil)
            while line
            do (dolist (wrapped (wrap-line line width))
                (write-line wrapped)))))

(let ((path (first ext:*args*))
      (width (second ext:*args*)))
  (if path
      (wrap-file path (if width (parse-integer width) 72))
      (write-line "usage: clisp wrap.lisp FILE [WIDTH]")))
