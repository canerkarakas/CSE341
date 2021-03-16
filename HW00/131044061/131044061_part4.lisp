(defun read-file-as-lines (filename)
    (with-open-file (in filename)
        (loop for line = (read-line in nil)
            while line
                collect line)
    )
)

(defun zerosArray (array size)
    (dotimes (index size)
        (setf (aref array index) 0)
    )
    array
)

(defun fillAlphIntValues (array size)
	(dotimes (index size)
		(setf (aref array index) index)
	)
	array
)

(defun copyAlphabets(source copy size)
	(dotimes (index size)
		(setf (aref copy index) (aref source index))
	)
	copy
)

(defun findLetterFreq (filename)
    (let ((f (open filename :if-does-not-exist nil)))
        (let* ((index nil) (alphabets (make-array 9999)))
            (setf alphabets (zerosArray alphabets 9999))
            (when f
                (loop for ch = (read-char f nil)
                    while ch do
                        (progn
                            (setf index (char-code ch))
                            (cond
                                ((and (>= index 0) (<= index 9999))
                                    (setf (aref alphabets index) (+ 1 (aref alphabets index)))
                                )
                            )
                        )
                )
                (close f)
            )
            alphabets
        )
    )
)

(defun findIndexList (source targetValue size)
	(let ((lst '()) (tempValue nil))
		(dotimes (index size)
			(setf tempValue (aref source index))
			(if (=  tempValue targetValue)
				(setf lst (append lst (list index)))
			)
		)
		lst
	)
)

(defun findZerosNumber (lst size)
    (let ((returnValue 0))
        (dotimes (index size)
            (if (= (aref lst index) 0)
                (setf returnValue (+ 1 returnValue))
            )
        )
        returnValue
    )
)

(defun discardZeros (lst size)
    (let ((zerosNumber (findZerosNumber lst size)))
        (let ((returnArray (make-array (- size zerosNumber))))
            (setf returnArray (zerosArray returnArray (- size zerosNumber)))
            (setf returnArray (copyAlphabets lst returnArray (- size zerosNumber)))
            returnArray
        )
    )
)

(defun sortingAlphabet (alphabets)
	(let ((tempLetters (fillAlphIntValues (make-array 9999) 9999)) (sorting_letters '()) (tempAlphabet (copyAlphabets alphabets (make-array 9999) 9999)))
		(setf tempAlphabet (stable-sort tempAlphabet #'>))
        (setf tempAlphabet (discardZeros tempAlphabet 9999))
        (dotimes (index (length tempAlphabet))
            (let ((value (findIndexList alphabets (aref tempAlphabet index) 9999)))
                (setf sorting_letters (append sorting_letters value))
                (setf index (+ index (length value)))
                (setf index (- index 1))
            )
        )
        (setf alphabets tempAlphabet)
        sorting_letters
	)
)

(defun arrToList (array)
    (let ((returnList '()))
        (dotimes (index (length array))
            (setf returnList (append returnList (list (aref array index))))
        )
        returnList
    )
)

(defstruct node
    letter
    value
    left_child
    right_child
    huffman_code_list
)

(defun make-letters (lst)
    (let ((returnList '()))
        (dotimes (index (length lst))
            (setf returnList (append returnList (list (nth index lst))))
        )
        returnList
    )
)

(defun start-make-letters-node-list (alp value)
    (let ((returnList '()))
        (dotimes (index (length alp))
            (setf returnList (append returnList (list (make-node :huffman_code_list '() :letter (code-char (nth index alp)) :value (nth index value)))))
        )
        returnList
    )
)


(defun remove-node-list (lst target)
    (let ((returnList '()))
        (dotimes (index (length lst))
            (if (not (eq (nth index lst) target))
                (setf returnList (append returnList (list (nth index lst))))
            )
        )
        returnList
    )
)

(defun append-node-list (lst target)
    (let ((returnList '()) (control T))
        (dotimes (index (length lst))
            (if (and (>= (node-value (nth index lst)) (node-value target)) (eq control T))
                (progn
                    (setf returnList (append returnList (list target)))
                    (setf returnList (append returnList (list (nth index lst))))
                    (setf control nil)
                )
                (setf returnList (append returnList (list (nth index lst))))
            )
        )
        (if (eq control T)
            (setf returnList (append returnList (list target)))
        )
        returnList
    )
)

(defun recTree (tree)
    (if (> (length tree) 1)
        (progn
            (setf (node-huffman_code_list (nth 0 tree)) (append (node-huffman_code_list (nth 0 tree))  (list 0)))
            (setf (node-huffman_code_list (nth 1 tree)) (append (node-huffman_code_list (nth 1 tree))  (list 1)))
            (setf tempNode (make-node :left_child (nth 0 tree) :right_child (nth 1 tree) :value (+ (node-value (nth 0 tree)) (node-value (nth 1 tree)))))
            (setf tree (append-node-list tree tempNode))
            (setf tree (cdr tree))
            (setf tree (cdr tree))
            (setf tree (recTree tree))
        )
    )
    tree
)

(defun  letters (root lstChars)
    (if (not (eq root nil))
        (progn
            (if (not (eq nil (node-letter root)))
                (setf lstChars (append lstChars (list (make-node :huffman_code_list (node-huffman_code_list root) :letter (node-letter root) :value (node-value root)))))
            )
            (setf lstChars (letters (node-right_child root) lstChars))
            (setf lstChars (letters (node-left_child root) lstChars))
        )
    )
    lstChars
)

(defun makeListForHufCodeLen (lst)
    (let ((returnList '()))
        (dotimes (index (length lst))
            (setf returnList (append returnList (list (length (node-huffman_code_list (nth index lst))))))
        )
        returnList
    )
)

(defun fixHuffmans (root lst)
    (if (not (eq root nil))
        (progn
            (setf (node-huffman_code_list root) (append lst (node-huffman_code_list root)))
            (fixHuffmans (node-right_child root) (node-huffman_code_list root))
            (fixHuffmans (node-left_child root) (node-huffman_code_list root))
        )
    )
)

(defun findAndAppendlist (lettersList resultList target)
    (dotimes (index (length lettersList))
        (if (= target (length (node-huffman_code_list (nth index lettersList))))
            (setf resultList (append resultList (list (nth index lettersList))))
        )
    )
    resultList
)

(defun makeTree ()
    (let ((alpValues (findLetterFreq "paragraph.txt")) (sortedAlp '()) (tree '()) (tempNode nil))
        (setf sortedAlp (sortingAlphabet alpValues))
        (setf alpValues (stable-sort alpValues #'>))
        (setf alpValues (discardZeros alpValues 9999))
        (setf alpValues (reverse alpValues))
        (setf sortedAlp (reverse sortedAlp))
        (setf alpValues (arrToList alpValues))
        (setf tree (start-make-letters-node-list sortedAlp alpValues))
        (setf tree (recTree tree))
        (setf tree (nth 0 tree))
        (fixHuffmans tree (node-huffman_code_list tree))
        tree
    )
)

(defun sortingLetterForWriting (tree)
    (let ((charsLst '()) (listforLen '()) (listSortedLetters '()) (tempLen 0))
        (setf charsLst (letters tree charsLst))
        (setf listforLen (makeListForHufCodeLen charsLst))
        (setf listforLen (stable-sort listforLen #'<))
        (dotimes (index (length listforLen))
            (setf tempLen (length listSortedLetters))
            (setf listSortedLetters (findAndAppendlist charsLst listSortedLetters (nth index listforLen)))
            (setf index (- (+ index (- (length listSortedLetters) tempLen)) 1))
        )
        listSortedLetters
    )
)

(defun listToStr (lst)
    (let ((str) (tempStr))
        (dotimes (index (length lst))
            (setf tempStr (concatenate 'string (write-to-string (node-letter (nth index lst))) ": "))
            (dotimes (index2 (length (node-huffman_code_list (nth index lst))))
                (setf tempStr (concatenate 'string str (write-to-string (nth index2 (node-huffman_code_list (nth index lst))))))
            )
        )
    )
)

(defun writeText (filename)
    (let ((sortedLetters (sortingLetterForWriting (makeTree))) (str))
        (with-open-file(stream filename :direction :output )
            (dolist (tempNode sortedLetters)
                (let ((str (write-to-string (node-letter tempNode))))
                    (setf str (concatenate 'string str ": "))
                    (dolist (i (node-huffman_code_list tempNode))
                        (setf str (concatenate 'string str (write-to-string i) " "))
                    )
                    (format stream str)
                    (terpri stream)
                )
            )
        )
    )
)

(writeText "huffman_codes.txt")
