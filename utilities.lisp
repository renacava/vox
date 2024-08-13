;;(in-package :vox)
(in-package :vox)


(defmacro funcall-if (form func predicate)
  "If predicate: return (funcall func form), else return form."
  `(if ,predicate
       (funcall ,func ,form)
       ,form))

(defmacro do-if (form modification predicate)
  "If predicate, return (modification form), else return form."
  `(if ,predicate
       (,modification ,form)
       ,form))

(defmacro enlamb (form)
  "Returns the given form wrapped in a lambda."
  `(lambda () ,form))

(defmacro enlamb-if (form predicate)
  "Returns (lambda () form) if predicate, else returns form."
  `(do-if ,form enlamb ,predicate))

(defmacro enlamb-plist (&rest plist)
  "Returns plist where each value is wrapped in a lambda."
  (let* ((plist (car plist))
         (just-items (if (eq (first plist) 'list)
                        (cdr plist)
                        plist)))
    (cons 'list
          (loop for item in just-items
           collect `(if (property-key? ,item)
                       ,item
                       (lambda () ,item))))))

(defmacro ntack (place obj)
  "Destructive version of tack"
  `(setf ,place (tack ,place ,obj)))

(defun flatten (structure)
  "Transforms any arbitrarily nested list into a flat list"
  (cond ((null structure) nil)
	((atom structure) (list structure))
	(t (mapcan #'flatten structure))))

(defun tack (place obj)
  "Tacks place at the end of obj, returns a proper list"
  (if (and
       (not (listp place))
       (not (listp obj)))
      (list place obj)
      (if (not (listp place))
	  (append (list place) obj)
	  (append place (cons obj nil)))))

(defun grow-list! (sequence amount &optional (initial-element 0.0))
  "Destructively appends n additional elements to the end of the given sequence, with given initial element. Returns said sequence."
  (nconc sequence (make-list amount :initial-element initial-element)))

(defun map-over! (sequence function)
  "Destructively applies the given function to each element in the given sequence, returns said sequence."
  (map-into sequence function sequence))

(defun last1 (sequence)
  "Returns the last element of a sequence"
  (car (last sequence)))

(defun same-at-index? (sequence1 sequence2 index)
  "Returns true if the elements at the given index of each sequence are equal."
  (equal (elt sequence1 index) (elt sequence2 index)))

(defun get-element-before-branch (sequence1 sequence2)
  "Scans two lists for their first difference, and returns the item before that.
Eg: (get-element-before-branch '(1 2 6 2) '(1 2 4 9)) => 2"
  (let ((last-common nil))
    (loop for i from 0 to (1- (shortest-length sequence1 sequence2)) do
          (if (same-at-index? sequence1 sequence2 i)
              (setf last-common (elt sequence1 i))
              (return)))
    last-common))

(defun add-margin (box margin)
  "Add some margin to a list of 4 elements, being the x, y, width and height of the box"
  (let ((half-margin (/ margin 2)))
    (list
     (- (elt box 0) half-margin)
     (- (elt box 1) half-margin)
     (+ (elt box 2) margin)
     (+ (elt box 3) margin))))

(defun range (length &optional (start-from 0))
  "Creates a list from start-from to length"
  (declare (fixnum length))
  (loop for i upto (- length 1) nconc (list (+ i start-from))))

(defun range-between (start end)
  "Creates a list from the given start to given end, "
  (when (> end start)
    (loop for i from start below end
          collect i)))

(defun range-from-list (sequence &optional (start 0))
  "Returns a list of numbers starting from given start to the length of the given sequence. (range-from-list '(2 4 5 7)) => (0 1 2 3)."
  (range (length sequence) start))

(defun shuffle (list)
  "Returns a random permutation of list"
  (let ((temp (copy-tree list)))
    (loop for i from (length temp) downto 2
	  do (rotatef (elt temp (random i))
		      (elt temp (1- i))))
    temp))

(defun to-hex (number)
  "Returns the given number after converting it to hexadecimal."
  (cond ((= number 0) "0")
	((= number 1) "1")
	((= number 2) "2")
	((= number 3) "3")
	((= number 4) "4")
	((= number 5) "5")
	((= number 6) "6")
	((= number 7) "7")
	((= number 8) "8")
	((= number 9) "9")
	((= number 10) "A")
	((= number 11) "B")
	((= number 12) "C")
	((= number 13) "D")
	((= number 14) "E")
	((= number 15) "F")
	(t number)))

(defun create-uuid (&optional (return-symbol?))
  "Creates a 128-bits universally unique identifier"
  (flet ((make-hex-string (length)
           (let ((result nil))
             (dotimes (i length)
               (setf result (concatenate 'string result (to-hex (random 16)))))
             (return-from make-hex-string result))))
    (let ((result
            (concatenate 'string
                         (make-hex-string 8)
                         "-"
                         (make-hex-string 4)
                         "-4"
                         (make-hex-string 3)
                         "-"
                         (elt (assoc (random 4)
                                     '((0 "8")
                                       (1 "9")
                                       (2 "A")
                                       (3 "B"))) 1)
                         (make-hex-string 3)
                         "-"
                         (make-hex-string 12))))
      (if return-symbol?
          (read-from-string result)
          result))))

(defun lerp (value-0 value-1 percentage)
  "Linear interpolation from value-0 to value-1, based on percentage"
  (+ value-0 (* percentage (- value-1 value-0))))

(defun lerp-number-list (list-0 list-1 percentage)
  "Linearly interpolates between two lists of numbers, based on percentage"
  (mapcar (lambda (x y) (lerp x y percentage)) list-0 list-1))

(defun lerp-number-list-3 (list1 list2 list3 percentage)
  (if (< percentage 0.5)
      (add-lists (list-multiply list1 (* 2 (- 0.5 percentage)))
                 (list-multiply list2 (* percentage 2)))
      (add-lists (list-multiply list3 (* 2 (- percentage 0.5)))
                 (list-multiply list2 (* 2 (- 1 percentage))))))

(defun list-multiply (list amount)
  (mapcar (lambda (x) (* x amount)) list))

(defun change-range (input initial-range-start initial-range-end final-range-start final-range-end)
  "Changes the range of the input based on its initial range and its final range"
  (+ final-range-start
     (* (- input initial-range-start)
	(/ (- final-range-end final-range-start)
	   (- initial-range-end initial-range-start)))))

(defun random-between (range-start range-end)
  "Returns a random value between range-start and range-end"
  (change-range (random 1.0) 0 1 range-start range-end))

(defun memoize (fun)
  "Returns a memoized version of fun, in which inputs have their outputs cached for faster retrival of already-computed results"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fun args)))))))

(defun sort-copy (sequence predicate)
  "Sort a sequence based on predicate, the original sequence stays untouched"
  (sort (copy-tree sequence) predicate))

(defun enlist (item)
  "Transforms item into a list if it isn't one already"
  (if (listp item)
      item
      (list item)))

(defun elt-bound (sequence index)
  "Returns nil if elt is out-of-bounds"
  (cond ((> 0 index) nil)
	((< (length sequence) index) nil)
	(t (elt sequence index))))

(defun elt-within (sequence index)
  "Returns the item at the given index, giving the last item in the list if the index is greater than the sequence's size, and the first item if the index is less than 0."
  (elt sequence (bound-number index 0 (1- (length sequence)))))

(defun elt-random (sequence)
  "Selects a random element of sequence"
  (when sequence
    (elt-bound sequence (random (length sequence)))))

(defun elt-random-n (sequence n)
  "Returns a list of size n (up to the length of the given sequence), where each element is a random element from the given sequence."
  (subseq (shuffle sequence) 0 (bound-number n 0 (length sequence))))

(defun elt-random-upto (sequence upto-n)
  "Selects a random element of a sequence, up to the given upto-n boundary."
  (elt sequence (min (random upto-n) (random (length sequence)))))

(defun but-last (sequence)
  "Returns sequence without its last element"
  (when (= 0 (length sequence))
    (return-from but-last (list nil)))
  (subseq sequence 0 (1- (length sequence))))

(defun list-to-string (lst)
  "Returns the given list after converting it to a string"
  (format nil "~{~A~}" lst))

(defun string-to-list (my-cool-string)
  "Returns a list of chars from the given string"
  (loop for c across my-cool-string
        collect c))

(defun index-fun (function sequence)
  "Reduces the sequence using the function and gives the index of the result in the sequence"
  (position (reduce function sequence) sequence))

(defun max-index (sequence)
  "Finds the index of the element with the highest value"
  (index-fun #'max sequence))

(defun min-index (sequence)
  "Finds the index of the element with the lowest value"
  (index-fun #'min sequence))

(defun between (number min-bound max-bound)
  "Returns true when number is between min-bound and max-bound, inclusively"
  (when (numberp number)
    (and (>= number min-bound) (<= number max-bound))))

(defun wrap (number min-bound max-bound)
  "Roll-over a number going beyond min-bound and max-bound"
  (if (between number min-bound max-bound)
      number
      (+ (mod number max-bound) min-bound)))

(defun bound-number (number min-bound max-bound)
  "Return number if between min-bound and max-bound, else return max-bound if greater, else min-bound if lesser."
  (cond ((> number max-bound) max-bound)
        ((< number min-bound) min-bound)
        (t number)))

(defun get-moore-neighbors (x y)
  "Returns the coordinates of the moore neighbors (The 8 squares around a square) of a given coordinate)"
  (list
   (list (- x 1) (- y 1)) (list (- x 1) y) (list (- x 1) (+ y 1))
   (list x (- y 1)) (list x (+ y 1))
   (list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))))

(defun hsv-to-rgb (input)
  "Takes a list of 3 values, corresponding respectively to the Hue, saturation and value of a colour, and transforms it into Red, Green and Blue values. The hue must be between 0 and 360, the saturation and value must be between 0.0 and 1.0"
  (let* ((c (* (elt input 2) (elt input 1)))
	 (x (* c (- 1 (abs (- (mod (/ (elt input 0) 60) 2) 1)))))
	 (m (- (elt input 2) c)))
    (let ((temp
	    (cond ((and (>= (elt input 0) 0) (< (elt input 0) 60)) (list c x 0))
		  ((and (>= (elt input 0) 60) (< (elt input 0) 120)) (list x c 0))
		  ((and (>= (elt input 0) 120) (< (elt input 0) 180)) (list 0 c x))
		  ((and (>= (elt input 0) 180) (< (elt input 0) 240)) (list 0 x c))
		  ((and (>= (elt input 0) 240) (< (elt input 0) 300)) (list x 0 c))
		  ((and (>= (elt input 0) 300) (< (elt input 0) 360)) (list c 0 x)))))
      (list (* (+ (elt temp 0) m) 255)
	    (* (+ (elt temp 1) m) 255)
	    (* (+ (elt temp 2) m) 255)))))

(defun elt-change (sequence index value)
  "Non-destructively changes an element in a sequence"
  (let ((temp (copy-tree sequence)))
    (append (tack (subseq temp 0 index) value) (subseq temp (+ 1 index)))))

(defun elt-insert (sequence index value)
  "Non-destructively inserts an element in a sequence"
  (let ((temp (copy-tree sequence)) (true-index index))
    (when (< true-index 0)
      (setf true-index 0))
    (when (> true-index (length sequence))
      (setf true-index (length sequence)))	     
    (append (tack (subseq temp 0 true-index) value) (subseq temp true-index))))

(defun elt-remove (sequence index)
  "Non-destructively removes an element in a sequence"
  (let ((temp (copy-tree sequence)))
    (append (subseq temp 0 index) (subseq temp (+ 1 index)))))

(defun repeat (f input times)
  "Repeats the application of the function f times on the input"
  (if (zerop times)
      input
      (repeat f (funcall f input) (- times 1))))

(defun keep-based-on (sequence1 sequence2)
  (map 'list (lambda (a b) (unless (null b) a)) sequence1 sequence2))

(defun keep-if-index (predicate sequence)
  (remove-if #'null (keep-based-on sequence (mapcar predicate (range (length sequence))))))

(defun mapcar-plist (function list)
  "Applies a function over a plist, skipping over the keywords"
  (mapcar (lambda (i) (if (keywordp i) (identity i) (funcall function i))) list))

(defun number-length (number)
  "Number of digits in the supplied number"
  (ceiling (log number 10)))

(defun explode (number)
  "Creates a list of the digits in a number, little-endian"
  (defun explode-helper (number index)
    (truncate (/ (mod number (expt 10 (+ index 1))) (expt 10 index))))
  (mapcar (lambda (i) (explode-helper number i)) (range (number-length number))))

(defun concatenate-numbers (a b)
  (+ (* a (expt 10 (number-length b))) b))

(defun de-explode (list)
  "Create a number from a little-endian list of numbers"
  (reduce (lambda (a b) (concatenate-numbers b a)) list))

(defun binary-list (number &optional acc)
  "Give the bits of a number in big-endian"
  (cond ((zerop number) (or acc (list 0)))
	((plusp number) (binary-list (ash number -1) (cons (logand 1 number) acc)))
	(t (error "~S: non-negative argument required, got ~s" 'binary-list number))))

(defun left-pad-t (sequence times &optional (padding 0))
  "Pads the sequence on the left times number of times"
  (let ((result (copy-seq sequence)))
    (dotimes (i times)
      (setf result (elt-insert result 0 padding)))
    result))

(defun left-pad-l (sequence desired-length &optional (padding 0))
  "Pads the sequence on the left until its length is equal to desired-length"
  (left-pad-t sequence (max 0 (- desired-length (length sequence))) padding))

(defun left-pad (sequence &key desired-length times (padding 0))
  "Pads the sequence on the left, either with the desired length, or the number of times to pad, not both"
  (if (or (and desired-length times) (and (not desired-length) (not times)))
      (error "Can't set both a :desired-length and :times in left pad")
      (if times
	  (left-pad-t sequence times padding)
	  (left-pad-l sequence desired-length padding))))

(defun right-pad-t (sequence times &optional (padding 0))
  "Pads the sequence on the right times number of times"
  (let ((result (copy-seq sequence)))
    (dotimes (i times)
      (ntack result padding))
    result))

(defun right-pad-l (sequence desired-length &optional (padding 0))
  "Pads the sequence on the right until its length is equal to desired-length"
  (right-pad-t sequence (max 0 (- desired-length (length sequence))) padding))

(defun right-pad (sequence &key desired-length times (padding 0))
  "Pads the sequence on the right, either with the desired length, or the number of times to pad, not both"
  (if (or (and desired-length times) (and (not desired-length) (not times)))
      (error "Can't set both a :desired-length and :times in right pad")
      (if times
	  (right-pad-t sequence times padding)
	  (right-pad-l sequence desired-length padding))))

(defun binary-list-to-decimal (list)
  "Transforms a list of binary numbers into its decimal representation"
  (reduce (lambda (x y) (+ (* 2 x) y)) list))

(defun copy-file-to (from-file to-file)
  "Copies the from-file and writes its contents into the to-file, creating if necessary."
  (with-open-file (input-stream from-file
                                :direction :input
                                :element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
      (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
        (loop for pos = (read-sequence buf input-stream)
              while (plusp pos)
              do (write-sequence buf output-stream :end pos))))))

(defun save-file (data filename)
  "Saves the Lisp expression 'data' to the path 'filename'"
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print data out))))

(defun load-file (filename)
  "Loads the lisp expression from the file at 'filename'"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun files-in-directory (&optional (directory "./"))
  "Returns a list of the files in directory"
  (directory (concatenate 'string directory
			  (unless (eq #\/ (elt directory (1- (length directory))))
			    "/")
			  "*.*")))

(defun is-file? (pathname)
  "Returns t if given pathname points to a file, else nil."
  (when (pathname-name pathname) t))

(defun is-file-in-folder? (pathname folder-name)
  "Returns t if given pathname points to a file that's in the given folder-name."
  (when (is-file? pathname)
    (equal folder-name (last1 (pathname-directory pathname)))))

(defun append-to-file (data filename)
  "Opens a file to the path 'filaname' and append something to it"
  (with-open-file (out filename :direction :output
				:if-exists :append
				:if-does-not-exist :create)
    
    (format out "~a~%" data)))

(defun sort-with-predicate (sequence predicate)
  "Non-destructively sorts a list via the provided predicate, returns a list.
If a nested list is provided, sorts based on the first entry of each list.
eg: (sort-with-predicate #'> (list 3 1 (list 2 4 9 5))) => (3 (2 4 9 5) 1)"
  (sort-copy sequence (lambda (i j) (funcall predicate (first (enlist i)) (first (enlist j))))))

(defun sort-descend (sequence)
  "Non-destructively sorts a list in descending order, returns a list."
  (sort-with-predicate sequence #'>))

(defun sort-ascend (sequence)
  "Non-destructively sorts a list in ascending order, returns a list."
  (sort-with-predicate sequence #'<))

(defun select-larger-list (sequence1 sequence2)
  "Returns the larger of two lists"
  (if (> (length sequence1) (length sequence2))
      sequence1
      sequence2))

(defun select-smaller-list (sequence1 sequence2)
  "Returns the smaller of two lists."
  (if (< (length sequence1) (length sequence2))
      sequence1
      sequence2))

(defun longest-length (sequence1 sequence2)
  "Returns the length of the largest of the two lists."
  (length (select-larger-list sequence1 sequence2)))

(defun shortest-length (sequence1 sequence2)
  "Returns the length of the smallest of two lists."
  (length (select-smaller-list sequence1 sequence2)))

(defun smaller-than-index (sequence value &optional (comparaison-function #'<))
  "Finds the index of the first element for which value is smaller than it"
  (let ((length (length sequence)))
    (labels ((smaller-helper (sequence value index)
	       (if (null sequence)
		   length
		   (if (funcall comparaison-function (car sequence) value)
		       (smaller-helper (cdr sequence) value (1+ index))
		       index))))
      (smaller-helper sequence value 0))))

(defun insertion-sort (sequence value &optional (comparaison-function #'<))
  "Inserts an element in a list according to the comparaison-function (Defaults to <)"
  (elt-insert sequence (smaller-than-index sequence value comparaison-function) value))

(defun t-or-nil (value)
  "Returns t if value is not nil, else nil."
  (when value t))

(defun unnest (value)
  "If the given value is a list, and the only element of that list is another list, then it returns the inner list, else returns given value."
  (if (listp value)
      (if (and (listp (first value)) (= (length value) 1))
          (first value)
          value)
      value))

(defun ramp-segmentation (sequence)
  "Segments a sequence into equally spaced terms"
  (let ((length (length sequence)))
    (mapcar (lambda (i) (list (+ (/ length) (/ i length)) (elt sequence i))) (range length))))

(defun ramp (sequence value)
  "Associate a value between 0.0 and 1.0 with an element in the sequence based on their order"
  (cond ((< value 0) (car sequence))
        ((>= value 1) (last1 sequence))
        (t (cadr (assoc value (ramp-segmentation sequence) :test #'<)))))

(defun contains? (object sequence &key (test #'eq))
  "Returns t if sequence contains object, else nil."
  (t-or-nil (find object sequence :test test)))

(defun string-contains? (string substring)
  "Returns t if given string contains given substring, else nil."
  (t-or-nil (search substring string)))

(defun string-empty? (str)
  "Returns T if str doesn't contain at least one character"
  (typecase str
    (string (= 0 (length str)))
    (t nil)))

(defun string-upcase-if (string predicate)
  "Returns the upcased version of the given string if the given predicate returns T, else returns the original string."
  (if predicate (string-upcase string) string))

(defun string-upcase-unless (string predicate)
  "Returns the upcased version of the given string unless the given predicate returns T, else returns the original string."
  (string-upcase-if string (not predicate)))

(defun string-starts-with? (string substring &optional (case-sensitive? nil))
  "Returns T if the given string begins with the given substring."
  (let ((string (string string))
	(substring (string substring)))
      (when (and
         (> (length substring) 0)
         (>= (length string) (length substring)))
    (string= (string-upcase-unless string case-sensitive?)
             (string-upcase-unless substring case-sensitive?)
             :end1 (length substring)))))

(defun string-replace-char (src-string char-to-replace char-to-insert)
  "Returns a copy of the given string after replacing all occurrences of char-to-replace with char-to-insert, in src-string."
  (let ((rep (if (characterp char-to-replace)
                 char-to-replace
                 (char char-to-replace 0)))
        (ins (if (characterp char-to-insert)
                 char-to-insert
                 (char char-to-insert 0))))
    (list-to-string
     (loop for current-char across src-string
           collect (if (char= current-char rep)
                       ins
                       current-char)))))

(defun boolp (value)
  "Returns T if the given value is of type boolean"
  (or (equal (type-of value) 'boolean)
      (null value)))

(defun to-string (x)
  "Returns the given x, after turning it into a string."
  (format nil "~a" x))

(defun set! (object value)
  "Use this when setf isn't working inside of a func.
It works because Common Lisp passes everything by value, not by reference, except for car, cdr and some other things which pass places. Rplaca and Rplacd work on places, letting us hack a way to pretend we passed something by reference."
  (rplaca (enlist object) (car (enlist value)))
  (rplacd (enlist object) (cdr (enlist value))))

(defun length= (sequence length)
  "Returns true if the given sequence is of the given length"
  (length-test sequence length #'=))

(defun length<= (sequence length)
  "Returns true if the given sequence is less than or equal to the given length"
  (length-test sequence length #'<=))

(defun length>= (sequence length)
  "Returns true if the given sequence is greater than or equal to the given length"
  (length-test sequence length #'>=))

(defun length< (sequence length)
  "Returns true if the given sequence is less than the given length"
  (length-test sequence length #'<))

(defun length> (sequence length)
  "Returns true if the given sequence is greater than the given length"
  (length-test sequence length #'>))

(defun length-test (sequence length test)
  "Returns true if the given sequence's length passes the given test"
  (when (listp sequence)
    (funcall test (length sequence) length)))

(defun abs- (value1 value2)
  "Returns the absolute difference of the two given values"
  (abs (- value1 value2)))

(defun interleave (a b)
  "Returns an interleaved list, eg: (list a b c) (list 1 2 3) => (a 1 b 2 c 3)"
  (flet ((nil-pad (list on-list)
           (append list (make-list (max 0 (- (length on-list) (length list)))))))
    (loop for x in (nil-pad a b)
          for y in (nil-pad b a)
          append (list x y))))

(defun pair-up (seq1 seq2)
  "Returns a list of pairs, where each item in seq1 is paired to each item in seq2."
  (let ((pairs))
    (dolist (outer seq1)
      (dolist (inner seq2)
        (ntack pairs (list outer inner))))
    pairs))

(defun string-to-list (my-cool-string)
  "Returns a list of chars from the given string"
  (loop for char across my-cool-string
	collect char))

(defun intersperse-list (sequence object)
  "Places the given object between each item in the given sequence, eg: (1 2 3) / => (1 / 2 / 3 /)"
  (loop for item in sequence
        nconcing (list item object)))

(defun print-table (data width)
  "Prints a table of width with the given data."
  (format t "~{|~{ ~{~Vd~}~}|~%~}"
          (mapcar #'(lambda (r) (mapcar #'(lambda (v) (list width v)) r)) data) ))

(proclaim '(inline x-if-nil zero-if-nil))

(defun x-if-nil (value x)
  "Returns x when the given value is nil, else returns the given value."
  (or value x))

(defun zero-if-nil (value)
  "Returns 0 if the given value is nil, else returns given value."
  (x-if-nil value 0))

(defun x-if-y (value x y &key (test #'=))
  "Returns the x if value = y."
  (if (funcall test value y)
      x
      value))

(defun binary-search-list (value sequence &optional (accessor nil) )
  "Returns the index of the given value from the given sequence, comparing the value to items in the list with the given accessor function, otherwise just comparing the items of the list directly to the given value. If not found, returns nil."
  (let ((low 0)
        (high (1- (length sequence)))
        (descending? (> (if accessor
                            (funcall accessor (first sequence))
                            (first sequence))
                        (if accessor
                            (funcall accessor (last1 sequence))
                            (last1 sequence)))))

    ;;reverse sequence if its in descending order
    (when descending?
      (setf sequence (reverse sequence)))

    ;; actual binary search
    (do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))
        
        (cond ((> (if accessor
                      (funcall accessor (elt sequence middle))
                      (elt sequence middle))
                  value)
               (setf high (1- middle)))
              
              ((< (if accessor
                      (funcall accessor (elt sequence middle))
                      (elt sequence middle))
                  value)
               (setf low (1+ middle)))
              
              (t (return (if descending?
                             (- (length sequence) middle 1)
                             middle))))))))

(defun to-property (value)
  "Returns the given value as a symbol prefixed with :, eg: 5 => :5, 'cat => :cat,! \"dog\" => :dog"
  (read-from-string (format nil ":~a" value)))

(defun to-property (value)
  "Returns the given value as a symbol prefixed with :, eg: 5 => :5, 'cat => :cat,! \"dog\" => :dog"
  (if (stringp value)
      (intern (string-upcase value) "KEYWORD")
      (intern (string-upcase (string value)) "KEYWORD"))
  
  ;;(read-from-string (format nil ":~a" value))
  )

(defun string-to-symbol (string)
  "Returns the given string as a symbol"
  (if (symbolp string)
      string
      (ignore-errors (read-from-string (string-replace-char (format nil "~a" string) " " "-")))))

(defun string-to-property (string)
  "Returns a symbol prexfixed with :, such that string 'dog' => :dog. Useful for procedurally accessing/setting property lists."
  (to-property string))

(defun get-property (sequence indicator)
  "Returns the property from the given property-list sequence, as indicated by the indicator. (get-property (list :x 10 :y 20) :y) => 20."
  (getf sequence (if (property-key? indicator)
                     indicator
                     (to-property indicator))))

(defun subseq-after (sequence delimiter &key (test))
  "Returns the given sequence or string, from after the given delimiter such that (subseq-after 'doggy' 'd') => 'oggy'"
  (unless test
    (typecase sequence
      (string (setf test #'string=))
      (list (setf test #'equal))))
  
  (let ((found-pos (position delimiter sequence :test test)))
    (subseq sequence (if found-pos
                         (1+ found-pos)
                         0))))

(defun subseq-before (sequence delimiter &key (test))
  "Returns the given sequence upto the given delimiter, such that (subseq-before (list 1 2 3 4 5) 3) => (1 2)"
  (reverse (subseq-after (reverse sequence) delimiter :test test)))

(defun concat-symbols (symbol-list)
  "Returns a symbol that is the concatenation of the given symbols, eg (concat-symbols (list 'cat 'dog)) => CATDOG"
  (apply #'symb symbol-list))

(defun property-key? (symbol)
  "Returns T if the given symbol is prefixed with :"
  (keywordp symbol))

(defun extract-property-keys (sequence)
  "Returns a list of symbols that are the keys for the given property list."
  (loop for item in sequence
        when (keywordp item)
        collect item))

(defun extract-property-list (sequence)
  "Extracts the property-key / value pairs from the given list, returning a property list from a malformed one. Unless you give a list with no property-key/value pairs, in which case it returns nil."
  (unless (listp sequence)
    (return-from extract-property-list nil))
  (let ((length (1- (length sequence))))
    (loop for item in sequence
          for index from 0
          when (and
                (keywordp item)
                (< index length)
                (not (keywordp (elt sequence (1+ index)))))
          collect (elt sequence index)
          and
          collect (elt sequence (1+ index)))))

(defun extract-not-property-list (sequence)
  "Extracts all elements in the given sequence that aren't paired with a :key. Unless you give a list with no property-key/value pairs, in which case it returns nil."
  (unless (listp sequence)
    (return-from extract-not-property-list nil))
  (let* ((indices (range (length sequence)))
         (length (1- (length sequence)))
         (property-indices (loop for item in sequence
                                 for index from 0
                                 when (and
                                       (keywordp item)
                                       (< index length))
                                 collect index
                                 and
                                 collect (1+ index)))
         (indices (set-difference indices property-indices)))
    (case (length indices)
      (0 nil)
      (1 (elt sequence (first indices)))
      (t (mapcar (lambda (index) (elt sequence index)) indices)))))

(defun extract-values-from-plists (plists property)
  "Grabs the value of the given property in all the given plists. 
   eg: (extract-properties-from-plists '((:x 10) (:x 2) (x 4)) :x -> '(10 2 4)"
  (mapcar (lambda (sequence) (getf sequence property))
          (enlist plists)))

(defun remove-property-keys (sequence)
  "Returns the given sequence with all :keys removed."
  (remove-if #'property-key? sequence))

(defmacro toggle! (value)
  "Destructively sets the given boolean value to (not value)"
  `(setf ,value (not ,value)))

(defun decimate (num decimal-place)
  "Returns the given number decimated to the nth decimal place, eg (decimate 0.123456789 5) => 0.12345"
  (float (* (truncate num (* 1.0 (/ 1 (expt 10 decimal-place)))) (/ 1 (expt 10 decimal-place)))))

(defun max-list (sequence)
  "Returns the biggest number in the given sequence."
  (reduce #'max sequence))

(defun min-list (sequence)
  "Returns the smallest number in the given sequence."
  (reduce #'min sequence))

(defun elt-n (sequence index &rest other-indices)
  "Returns the elt of the elt of the elt of etc, of the given sequence. Eg: (elt-n my-cool-list 0 1 2) == (third (second (first my-cool-list))) == myCoolList[0][1][2]"
  (let ((indices (append (enlist index) other-indices))
        (current-list (copy-tree sequence)))
    (dolist (current-index indices)
      (setq current-list (elt current-list current-index)))
    current-list))

(defun call-if-func (value)
  "Calls the given value if its a function, else returns nil."
  (when (functionp value)
    (funcall value)))

(defmacro setf-if-nil (place value)
  "Setf's the given place to the given value UNLESS it already has a value. value only evaluates if place is nil."
  `(if ,place
       ,place
       (setf ,place ,value)))

(defmacro setf-unless-nil (place value)
  "Assigns the given value to the given place, unless the value is nil. Returns the value of place."
  `(if ,value
       (setf ,place ,value)
       ,place))

(defmacro mac1 (expr)
  "Pretty prints the macroexpansion-1 of the given expression."
  `(pprint (macroexpand-1 ',expr)))

(defun property-to-symbol (property)
  "Converts a :property into a 'symbol."
  (symb property))

(defun symbol-to-string-preserve-quotes (symbol)
  "Returns the given symbol as a string, where double-quotes are turned into \"'s"
  (with-output-to-string (s)
    (print symbol s)))

(defmacro bracketise-properties-as-vars (&body property-list)
  "Returns a list of symbols where each symbol is '(property-key-without-colon value)"
  `(let ((props (extract-property-list ',@property-list))
         (result nil))
     (dolist (item (extract-property-keys props))
       (ntack result `(,(property-to-symbol item) ,(getf props item))))
     result))

(defmacro property-pair-to-var (&body property-pair)
  "(:x 10) => (x 10)"
  `(,(property-to-symbol (first property-pair)) ,(second property-pair)))

(defmacro new-plist* (plist)
  "Returns a plist whose arguments have been evaluated sequentially, such that latter items may defined in terms of items listed before itself."
  ;; remove first item in plist if it's not a property
  (setf plist (extract-property-list plist))

  (let
      ;; (:x 10 :y 20 :z (* x y)) => ((x 10) (y 20) (z (* x y)))
      ((quoted-plist (flet ((gather-keys ()
                              (let ((result))
                                (dotimes (index (length plist))
                                  (when (evenp index)
                                    (ntack result `(,(read-from-string (format nil "~a" (elt plist index))) ,(elt plist (1+ index))))))
                                result)))
                       (gather-keys)))

       ;;(:x 10 :y 20 :z (* x y)) => (list :x x :y y :z z)
       (key-pairs (flet ((gather-key-pairs ()
                           (let ((result (list 'list)))
                             (dotimes (index (length plist))
                               (if (evenp index)
                                   (ntack result (elt plist index))
                                   (ntack result (read-from-string (format nil "~a" (elt plist (1- index)))))))
                             result)))
                    (gather-key-pairs))))
    `(let* ,quoted-plist ,key-pairs)))

(defun resolve (value)
  "Repeatedly funcalls the given value if its a function, until it returns something that isn't a function. If value isn't a function to begin with, then it just returns value."
  (if (functionp value)
      (resolve (funcall value))
      value))

(defmacro resor (value &rest alternatives)
  "Returns the resolved value if not nil, else first non-nil alternative."
  `(or (resolve ,value) ,@alternatives))

(defun rezero (value)
  "Returns the (resolve value) if not nil, else 0."
  (or (resolve value) 0))

(defun call-if-func-recursively (value)
  "Recursively checks if the given value is a function, calling it if so, until the value returned from funcalling is not a function. Returns that value."
  (resolve value))

(defun cifr (value)
  "An abbreviated signature of call-if-func-recursively. Recursively calls the result of the given value if its a function, until a non-function is reached, returns that non-function value."
  (call-if-func-recursively value))

(defun divide-list (sequence width)
  "Returns a list of lists, that have been created by dividing the given sequence into subsequences of max length width."
  (unless (< 0 width)
    (setf width 1))
  (if (> (length sequence) width)
      (append (list (subseq sequence 0 width)) (divide-list (subseq sequence width) width))
      (list sequence)))

(defun nth-char (string n)
  "Returns the nth char in the given string."
  (elt (string-to-list string) n))

(defun first-char (string)
  "Returns the first char of the given string."
  (nth-char string 0))

(defun last-char (string)
  "Returns the last char in the given string."
  (last1 (string-to-list string)))

(defun last-index? (sequence index)
  "Returns t if the given index is equal to the length of the given sequence -1."
  (= index (1- (length sequence))))

(defun trim-left-whitespace (text)
  "Returns the given string with all whitespace prefixing it removed."
  (trim-whitespace text t))

(defun trim-right-whitespace (text)
  "Returns the given string with all whitespace after it removed."
  (trim-whitespace text))

(defun trim-whitespace (text &optional (left? nil))
  "Removes trailing/following whitespace from a string, specify left? as t for left-space, nil for right-space."
  (funcall (if left?
               #'string-left-trim
               #'string-right-trim)
           `(#\Space #\Newline #\Backspace #\Tab 
                     #\Linefeed #\Page #\Return #\Rubout)
           text))

(defun whitespace? (string)
  "Returns t when the given string consists of only whitespace."
  (when (and
         (< 0 (length (string string)))
         (= 0 (length (trim-whitespace (string string)))))
    t))

(defun remove-spaces (str)
  "Returns str with spaces removed"
  (coerce (loop for char across str
                unless (char= char #\ )
                collect char)
          'string))

(defun first-char-whitespace? (string)
  "Returns t when the first char of the given string is whitespace."
  (whitespace? (first-char string)))

(defun last-char-whitespace? (string)
  "Returns t when the last char of the given string is whitespace."
  (whitespace? (last-char string)))

(defun suffix-string (string suffix)
  "Returns the addition of the given suffix to the given string."
  (concatenate 'string string suffix))

(defun prefix-string (prefix string)
  "Returns the given string with the prefix prefixed to it."
  (concatenate 'string prefix string))

(defun suffix-string-if (string suffix predicate)
  "Returns the given string with the given suffix appended to it, if the predicate resolves to T, else returns the original string."
  (if (call-if-func-recursively predicate)
      (suffix-string string suffix)
      string))

(defun suffix-string-unless (string suffix predicate)
  "Returns the given string with the given suffix appended to it, unless the predicate resolves to T, else returns the original string."
  (suffix-string-if string suffix (not predicate)))

(defun next-item (sequence current-index)
  "Returns the next item in the given sequence, from the given current-index, bounded so if current-index is greater than or equal to the length of the sequence, it'll return the last item in the sequence instead of error-ing out."
  (elt sequence (bound-number current-index 0 (1- (length sequence)))))

(defun bound-func-result (func min &optional (max nil))
  "Returns a function which returns a bounded result of the given function. So, if you have a func that might return a value outside a bound you want it to, then use this function to essentially cap its results."
  (lambda () (let ((result (call-if-func-recursively func)))
               (cond 
                 ((> min result) min)
                 (max (if (< max result) max result))
                 (t result)))))

(defun remove-property-and-value (sequence property-key)
  "Removes the property and the value of it from a given property list."
  (let ((sequence (extract-property-list (copy-list sequence))))
    (if (getf sequence (to-property property-key))
        (remove (to-property property-key)
                (remove
                 (getf sequence
                       (to-property property-key))
                 sequence))
        sequence)))

(defun remove-properties-and-values (sequence property-keys)
  "Returns the given sequence after removing all properties and their values, as passed in property-keys, from the given sequence."
  (let ((result sequence))
    (dolist (key property-keys)
      (setf result (remove-property-and-value result key)))
    result))

(defun string-insert (destination-string string-to-insert insertion-index)
  "Inserts the given string-to-insert into the destination-string at the given insertion-index."
  (format nil "~a~a~a"
          (subseq destination-string 0 insertion-index)
          string-to-insert
          (subseq destination-string insertion-index)))

(defun string-remove-at-index (string index n-of-chars-to-remove)
  "Removes n-of-chars at the given index in the given string."
  (format nil "~a~a"
          (subseq string 0 index)
          (subseq string (+ index n-of-chars-to-remove))))

(defun tokenise (string delimiter)
  "its very important the delimiter is a char and not a string ok"
  (loop for start = 0 then (1+ finish)
        for finish = (position delimiter string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun stepify (value step)
  "Returns the given value rounded down to the value divided by the step. eg: (stepify 65.1235 50) -> 50. (stepify 65.1235 12) -> 60."
  (* (truncate (/ value step)) step))

(defun float-to-percentage (value &key (decimal-places 2) (min-step))
  "Returns the given float (0..1) converted to percentage, with the given number of decimal places and with a minimum step of the given min-step."
  (let ((result (* 100 value)))
    (if (= 0 decimal-places)
        (setf result (truncate result))
        (setf result (decimate result decimal-places)))

    (if min-step
        (stepify result min-step)
        result)))

(defun midpoint (x1 y1 x2 y2)
  "Returns the midpoint of the two points (x1 y1) (x2 y2), as (x-midpoint y-midpoint)"
  (list (* 0.5 (+ x1 x2))
        (* 0.5 (+ y1 y2))))

(defun rad-to-deg (radian-value)
  "Converts radians to degrees."
  (* radian-value (/ 180 pi)))

(defun deg-to-rad (degrees)
  "Converts degrees to radians."
  (* degrees (/ pi 180)))

(defun move-point-in-direction (x y angle distance)
  "Returns a list of x/y coordinates, where the given point (x, y) has moved DISTANCE in direction ANGLE."
  (list
   (+ x (* distance (cos (deg-to-rad angle))))
   (+ y (* distance (sin (deg-to-rad angle))))))

(defun hyp (x y)
  "Returns the hypotenuse of triangle with sides x,y,hyp"
  (sqrt (+ (* x x) (* y y))))

(defun angle (x y)
  "Returns the angle in radians from 0,0 to x,y on a cartesian plane"
  (asin (/ x (hyp x y))))

(defun angle-2-points (p1 p2)
  "Returns the angle between two points in degrees"
  (mod
   (rad-to-deg
    (atan (- (first p2) (first p1))
          (- (second p2) (second p1))))
   360))

(defun square (x)
  "Returns the square of x"
  (expt x 2))

(defun cube (x)
  "Returns the cube of x"
  (expt x 3))

(defmacro concatenate-non-nils (result-type &body sequences)
  "Concatenates all given items unless an item is nil. An item, if it's a list that contains other values besides nil, will still be concatenated. So long as the total value of any given item is not nil, it will be concatenated."
  (let ((new-seqs (remove-if #'null sequences)))
    `(concatenate ,result-type ,@new-seqs)))

(defun getf-last-in-plist (property-list indicator)
  "(getf-last (list :x 10 :x 20 :x 30)) -> 30."
  (let ((pos (position indicator property-list :from-end t)))
    (when pos
      (elt property-list (1+ pos)))))

(defun remove-duplicate-property-value-pairs (property-list &optional (from-end nil))
  "(list :x 10 :y 20 :x 30) => (list :y 20 :x 30)"
  (let ((keys (remove-duplicates (extract-property-keys property-list)))
        (result))
    (dolist (key keys)
      (ntack result key)
      (ntack result (funcall (if from-end
                                 #'getf-last-in-plist
                                 #'getf)
                             property-list key)))
    result))

(defun add-lists (list1 list2)
  "(add-lists (list 1 2) (list 3 4)) => (list 4 6)"
  (let* ((lists (sort (list list1 list2)
                      (lambda (x y) (> (length x)
                                       (length y)))))
         (shortest-list (last1 lists))
         (longest-list (first lists)))
    (dotimes (index (length shortest-list))
      (setf (elt longest-list index)
            (+ (elt longest-list index)
               (elt shortest-list index))))
    longest-list))

(defun invert-number-list (sequence)
  "(invert-number-list (list 1 -2 3)) => (list -1 2 -3)"
  (mapcar (lambda (x) (* -1.0 x)) sequence))

(defun string-max-length (string max-length &key (ellipse? t))
  "Returns the given string trimmed so it doesn't exceed the given max-length."
  (if (and ellipse?
           (> (length string) max-length))
      (setf string (format nil "~a..."
                           (subseq string 0 max-length)
                           ))
      (subseq string 0 (min (length string)
                            max-length))))

(defun mkstr (&rest args)
  "Returns a string composed of the given args."
  (with-output-to-string (s)
    (dolist (arg args)
      (princ arg s))))

(defun symb (&rest args)
  "Returns a symbol composed of the given args."
  (values (intern (apply #'mkstr args))))

(defun end-of-list? (sequence)
  "Returns T if the given sequence's cdr is nil."
  (when (listp sequence)
    (null (cdr sequence))))

(unless (boundp '*utilities-loaded-files*)
  (defparameter *utilities-loaded-files* (list t)))

(defun require-file (filename-string)
  "Loads the file corresponding to the given filename-string, unless it's already been loaded by require-file before."
  (if (find (string-upcase filename-string) *utilities-loaded-files*  :test #'equal)
      (format nil "file ~a already loaded" filename-string)
      (progn
        (load filename-string)
        (ntack *utilities-loaded-files* (string-upcase filename-string)))))


(defun string-combine (string1 glue string2)
  "Combines the two given strings, with the given glue as a string between them. eg: (string-combine 'cat' '-' 'dog') => 'cat-dog'."
  (format nil "~a~a~a" string1 (x-if-nil glue "") string2))

(defun string-endash (str1 str2)
  "Returns the given strings combined with a dash. (string-endash cat dog) => cat-dog"
  (string-combine str1 "-" str2))

(defun string-append-gensym (string)
  "Returns the given string after appending a - and a gensym."
  (string-endash string (gensym)))

(defun list-average (sequence)
  "Returns the average of the numbers in the given sequence."
  (* 1.0 (/ (loop for item in sequence
                  summing item)
            (length sequence))))

(defun get-func (func-name)
  "Returns the function with the given name, eg: (get-func \"my-cool-func\") -> <FUNCTION MY-COOL-FUNC>"
  (fdefinition (intern (string-upcase func-name))))

(defun half (num)
  "Returns the given number * 0.5"
  (* 0.5 num))

(proclaim '(inline singlep append1 nconc1))
(defun singlep (sequence)
  "Returns T if the given sequence has only 1 item."
  (and (consp sequence) (not (cdr sequence))))

(defun append1 (sequence object)
  "Appends a single non-list object to the end of the given sequence."
  (append sequence (list object)))

(defun nconc1 (sequence object)
  "nconc's the given non-list object to the end of the given sequence."
  (nconc sequence (list object)))


(defun filter (sequence func)
  "Returns a list of all non-nil return-values from applying func to each item in the given sequence. eg:(filter '((list :x 10) (list :x 12) (list :y 5)) (lambda (item) (getf item :x)) => '(10 12))"
  (let ((accumulation nil))
    (dolist (item sequence)
      (let ((result (funcall func item)))
        (when result
          (push result accumulation))))
    (nreverse accumulation)))

(defun group (sequence group-size)
  "Returns a list of sublists, where each sublist is of length group-size, and the last element of the list is the remainder. eg:(group '(1 2 3 4 5) 2) => ((1 2) (3 4) (5))"
  (if (zerop group-size) (error "Can't divide sequence into lists of length 0."))
  (labels ((recurse (seq accumulation)
             (let ((rest (nthcdr group-size seq)))
               (if (consp rest)
                   (recurse rest (cons (subseq seq 0 group-size) accumulation))
                   (nreverse (cons seq accumulation))))))
    (if sequence
        (recurse sequence nil)
        nil)))

(defun longer (sequence1 sequence2)
  "Returns T when sequence1 is longer than sequence2."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp sequence1)
             (listp sequence2))
        (compare sequence1 sequence2)
        (> (length sequence1) (length sequence2)))))

(defun flatten (tree)
  "Returns a list of all the atoms in the given tree (or any structure)"
  (labels ((rec (item acc)
             (cond ((null item) acc)
                   ((atom item) (cons item acc))
                   (t (rec (car item) (rec (cdr item) acc))))))
    (rec tree nil)))

(defun prune (test tree)
  "Returns the given tree after removing all atoms that pass the given test."
  (labels ((rec (inner-tree acc)
             (cond ((null inner-tree) (nreverse acc))
                   ((consp (car inner-tree))
                    (rec (cdr inner-tree)
                         (cons (rec (car inner-tree) nil) acc)))
                   (t (rec (cdr inner-tree)
                           (if (funcall test (car inner-tree))
                               acc
                               (cons (car inner-tree) acc)))))))
    (rec tree nil)))

(defun before-seq (x y sequence &key (test #'eql))
  "Returns the cdr of the given sequence if x occurs in it before y, else nil. Remember, you can test the return of this func for T, the cdr is just potentially useful extra info."
  (and sequence
       (let ((first (car sequence)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) sequence)
               (t (before x y (cdr sequence) :test test))))))

(defun after-seq (x y sequence &key (test #'eql))
  "Returns the cdr of the given sequence from x, if x occurs after y, and both x and y occur in the given sequence. Else nil."
  (let ((rest (before y x sequence :test test)))
    (and rest (member x rest :test test))))

(defun duplicate-p (object sequence &key (test #'eql))
  "Returns the remainder of the given sequence, if the given object occurs more than once in the given sequence."
  (member object (cdr (member object sequence :test test))
          :test test))

(defun split-if (func sequence)
  "Returns 2 values: the given sequence before and the given sequence after the given func returns T on an element in the given sequence. eg:(split-if #'oddp '(2 4 6 7 8 10 11 12 13) => (2 4 6) (7 8 10 11 12 3))"
  (let ((acc nil))
    (do ((source sequence (cdr source)))
        ((or (null source) (funcall func (car source)))
         (values (nreverse acc) source))
      (push (car source) acc))))

(defun most (func sequence)
  "Returns the item in the given sequence that returns the highest number when the given func is applied to it, along with the number the func returned. eg:(most #'length '((a b) (a b c) (a) (e f g))) => (A B C) 3. In case of ties, the first element wins."
  (if (null sequence)
      (values nil nil)
      (let* ((wins (car sequence))
             (max (funcall func wins)))
        (dolist (item (cdr sequence))
          (let ((score (funcall func item)))
            (when (> score max)
              (setq wins item
                    max score))))
        (values wins max))))

(defun best (func sequence)
  "Returns the item in the given sequence which, when the given func is applied to it, returned the highest number. Func must take two arguments (so we can compare items). eg:(best #'> '(5 2 8 4 9 1)) => 9. #'best can be thought of as (car (sort func sequence)), but a lot more efficient! In case of ties, the first element wins."
  (if (null sequence)
      nil
      (let ((wins (car sequence)))
        (dolist (item (cdr sequence))
          (if (funcall func item wins)
              (setq wins item)))
        wins)))

(defun mostn (func sequence)
  "Returns a list of the top n scoring items (and their score) in the given sequence, when func is applied to the item. This is like #'most, but it returns a list of ties instead of just the first occuring highest scoring item."
  (if (null sequence)
      (values nil nil)
      (let ((result (list (car sequence)))
            (max (funcall func (car sequence))))
        (dolist (item (cdr sequence))
          (let ((score (funcall func item)))
            (cond ((> score max)
                   (setq max score
                         result (list item)))
                  ((= score max)
                   (push item result)))))
        (values (nreverse result) max))))

(defun mapa-b (func a b &optional (step 1))
  "Applies the given func to a range of numbers from a to b, without consing up a list to contain them. eg:(mapa-b #'1+ 2 1 0.5) => (-1 -0.5 0.0 0.5 1.0)"
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall func i) result)))

(defun map0-n (func n)
  "Applies the given func to a range of numbers without consing up a list to contain them, from 0 to n. eg:(map0-n #'1+ 3) => (1 2 3 4)"
  (mapa-b func 0 n))

(defun map1-n (func n)
  "Applies the given func to a range of numbers without consing up a list to contain them, from 1 to n. eg:(map1-n #'1+ 3) => (2 3 4)"
  (mapa-b func 1 n))

(defun map-> (func start end-test-func incrementor-func)
  "Applies the given func to a sequence created from the given start, until an item is generated that satisfies the given end-test-func, using the incrementor-func to generate successive items for the sequence. eg:(map-> #'1+ -1 (lambda (x) (= x 2)) #'1+) => (0 1 2)"
  (do ((i start (funcall incrementor-func i))
       (result nil))
      ((funcall end-test-func i) (nreverse result))
    (push (funcall func i) result)))

(defun replace-at (lst obj n)
  "Returns the given list after replacing the object at n with the given obj."
  (append (append (subseq lst 0 n) (enlist obj))
          (subseq lst (1+ n))))

(defmacro list-to-plist (list-to-convert)
  "(let ((dog 5)) (list-to-plist (list dog) => (:DOG 5))"
  `(list
    ,@(interleave
       (mapcar #'to-property
               (mapcar (lambda (item)
                         (if (listp item)
                             (car item)
                             item))
                       (cdr list-to-convert))
               )
       (loop for item in (cdr list-to-convert)
             append item))))

(defmacro list-to-plist (list-to-convert)
  "(let ((dog 5)) (list-to-plist (list dog) => (:DOG 5))"
  `(list
    ,@(loop for item in (case (car list-to-convert)
                          ('quote (car (cdr list-to-convert)))
                          ('list (cdr list-to-convert)))
            append (list (to-property item) item))))

(defun hash-table-print (hash-table)
  "Prints the given hash-table to *standard-output*"
  (maphash (lambda (key value)
	     (format t "~a: ~a~%" key value))
	   hash-table))

(defun from-first-property (lst)
  "Returns a copy of the given list, starting from the first found :property."
  (let ((index 0))
    (loop for item in lst
          while (not (property-key? item))
          do (incf index))
    (unless (>= index (length lst))
      (subseq lst index))))

(defun until-first-property (lst)
  "Returns a copy of the given list, starting from 0 and ending before the first :property is found."
  (let ((prop-index (position t lst :test (lambda (x y) (property-key? y)))))
    (subseq lst 0 (or prop-index (length lst)))))


(defun property-list-override-n (src dst)
  "Returns the dst property-list with all items from src inserted and/or overwriting items in dst. This is destructive."
  (loop for prop in (extract-property-keys src)
        do (setf (getf dst prop) (getf src prop)))
  dst)

(defun property-list-override (src dst)
  "Returns a copy of the dst property-list with all items from src inserted and/or overwriting items in dst."
  (let ((src (copy-list src))
        (dst (copy-list dst)))
    (loop for prop in (extract-property-keys src)
          do (setf (getf dst prop) (getf src prop)))
    dst))

(defmacro plist-delay (plist)
  "Returns the given plist where all values of keys are wrapped in a lambda."
  (let ((lambdas (cons 'list (mapcar (lambda (item) (list 'lambda '() item)) plist))))
    `(let ((result))
       (dolist (lamb (mapcar (lambda (func-list) (compile nil func-list)) (cdr ,lambdas)))
         (ntack result (if (property-key? (funcall lamb))
                           (funcall lamb)
                           lamb)))
       result)))



(defun load-file-into-string-list (file-path)
  "Returns a list of strings, each string representing a line in the file pointed to by the given file-path."
  (let ((in (open file-path :if-does-not-exist nil))
	(lines))
    (when in
      (loop for line = (read-line in nil)
	    while line do (push line lines)))
    lines))

(defun eq-any (x &rest y)
  "Returns T when x is equal to at least one of the other given args."
  (loop for other in y
	when (eq x other)
        do (return-from eq-any t)))

(defun ensure-quote (value)
  "Returns the given value guaranteed to be prefixed by ', as a symbol"
  (let ((string (format nil "~a" value)))
    (string-to-symbol
     (if (eq (elt string 0) #\')
         string
         (prefix-string "'" string)))))



(defun lists-share-structure-p (list1 list2)
  "Return non-nil if the two lists share structure, otherwise nil. Assume non-cyclic."
  (and (consp list1)
       (consp list2)
       (eq (last list1)
           (last list2))))

(defun prefix-key (prefix key)
  (to-property (format nil "~a~a" prefix key)))

(defun prefix-keys (prefix keys)
  (mapcar (lambda (key) (prefix-key prefix key)) keys))

(defun prefix-keys-in-list (lst prefix)
  (loop for item in lst
        collect (if (keywordp item)
                    (prefix-key prefix item)
                    item)))

(defun set-unless-found (key hashtable func)
  "Sets the entry at key in hashtable to the return of func, unless an entry is found already. After, returns the value at key."
  (multiple-value-bind (result found?) (gethash key hashtable)
    (if found?
        result
        (setf (gethash key hashtable) (funcall func)))))

(defun elt-or (sequence index &optional alternative)
  (or (ignore-errors (elt sequence index)) alternative))

(defun path-is-directory? (path)
  (when path
    (let* ((path (format nil "~a"
                         (typecase path
                           (pathname (pathname path))
                           (t path))))
           (path-length (1- (length path))))
      (= (position #\/ path :from-end t)
         path-length))))

(defun file-exists? (path)
  (not (path-is-directory? (find-file path))))

(defun last1 (lst)
  (first (last lst)))

(defun num-from-string (value)
  (or (ignore-errors (read-if-string value)) 0))

(defun read-if-string (value)
  (typecase value
    (string (ignore-errors (read-from-string value)))
    (t value)))

(defun hash-table-to-list (table)
  "Returns a list where each item is the value of a key in table. Keys are lost, only a list of values is returned."
  (let (result)
    (maphash (lambda (key value) (push value result)) table)
    result))

(defun group-list-by-func-to-table (lst func &key (test #'equal))
  "Returns a table of items who all share the same result of calling func on them. As in, (funcall func item). Eg, (group-list-by-func-to-table (list 1 2 3 4 5) #'oddp) => {T:(4 2), nil:(5 3 1))}. Orders not guaranteed."
  (let ((table (make-hash-table :test test)))
    (loop for item in lst
          do (let ((key (funcall func item)))
               (multiple-value-bind (result found?) (gethash key table)
                 (setf (gethash key table)
                       (if found?
                           (push item result)
                           (list item))))))
    table))

(defun group-list-by-func (lst func &key (test #'equal))
  "Returns a lists of items who all share the same result of calling func on them. As in, (funcall func item). Eg, (group-list-by-func (list 1 2 3 4 5) #'oddp) => ((4 2) (5 3 1))). Orders not guaranteed."
  (hash-table-to-list (group-list-by-func-to-table lst func :test test)))

(defun filter-minimum-length (lsts n)
  "Returns a list of all sublists of lsts whose length is at least n."
  (remove-if-not (lambda (lst) (>= (length lst) n)) lsts))

(defun filter-maximum-length (lsts n)
  "Returns a list of all sublists of lsts whose lengths are no greater than n."
  (remove-if-not (lambda (lst) (<= (length lst) n)) lsts))

(defun remove-nils (lst)
  (loop for item in lst when item collect item))

(defun search-for-strings-in-list (strings lst &key (remove-whitespace? nil) (case-sensitive? t))
  "Returns t if all strings are found in lst, where lst is a list of strings."
  (setf strings (mapcar (lambda (x) (format nil "~a" x)) strings)
        lst (mapcar (lambda (x) (format nil "~a" x)) lst))
  (when remove-whitespace?
    (setf lst (mapcar #'remove-spaces lst)
          strings (mapcar #'remove-spaces strings)))
  (unless case-sensitive?
    (setf lst (mapcar #'string-downcase lst)
          strings (mapcar #'string-downcase strings)))
  
  (loop for str in strings
        ;;unless (find str lst :test #'string=)
        do (unless (find str lst :test #'string=)
             (return-from search-for-strings-in-list nil)))
  t)

;; path-to-function breaks utilities.lisp when loading on sbcl 2.3.x; though it works fine on sbcl 2.0.0.
;; (defun path-to-function (function-name)
;;   "Returns a pathname to the given #'function-name, relies on SBCL sb-introspect."
;;   (sb-introspect:definition-source-pathname (sb-introspect:find-definition-source function-name)))


;;(let ((pack (find-package :utilities)))
;;  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
