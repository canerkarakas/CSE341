
(defun line-to-string-list (line)
    (if (not (streamp line))
       (line-to-string-list (make-string-input-stream line))
        (if (listen line)
           (cons (read line) (line-to-string-list line))
        )
    )
)

(defun read-file-as-lines (filename)
	(with-open-file (in filename)
		(loop for line = (read-line in nil nil)
			while line
				collect line)
	)
)

(defun func (filename outputFilename)
    (let ((f (open filename :if-does-not-exist nil)))
    	(with-open-file (stream outputFilename :direction :output )
	        (let* ((lines '()) (index nil) (tempChar nil) (l '()) (str nil) (input_list '()))
	            (setf lines (read-file-as-lines filename))
	            (close f)
	            (loop for line in lines
	                do (progn
		                    (if (< 1 (length line))
		                    	(progn
                    				(setf l (line-to-string-list line))
                    				(setf input_list (append input_list l))
		                    	)
		                    )
	                	)
	            )
	            input_list
	        )
	    )
	)
)

(defun function_prolog(input_list)
	(let ((facts '()) (clauses '()) (queries '()) (return_list '()))
		(dotimes (line (length input_list))
			(if (equal (nth 0 (nth line input_list)) nil)
				(setf queries (append queries (list (nth 1 (nth line input_list))) ) )
				(progn
					(if (equal (nth 1 (nth line input_list)) nil)
						(setf facts (append facts (list (nth 0 (nth line input_list)))))
						(setf clauses (append clauses (list (nth line input_list))))
					)
				)
			)
		)
		(dotimes (queryIndex (length queries))
			(setf return_list (append return_list (list (find_query_result facts clauses (nth queryIndex queries)))))
		)
		return_list
	)
)

(defun find_query_result (facts clauses query)
	(let ((clausesFor (find_clause clauses query)) (return_clause_result nil))
		(setf return_clause_result (append return_clause_result (clause_result facts (car clausesFor) clauses query)))
		(setf clausesFor (cdr clausesFor))
		(if (and (equal return_clause_result nil) (not (equal nil clausesFor)))
			(setf return_clause_result (find_query_result facts (cdr clausesFor) query))
			return_clause_result
		)
	)
)
 
(defun clause_result (facts clause clauses query)
	(let ((return_clause_result nil) (tempClauses '()) (tempFacts '()))
		(setf tempFacts (append tempFacts (nth 1 clause)))
		(dotimes (factsIndex (length tempFacts))
			(let ((tempFactLeft '()) (tempFactRight '()))
				(setf tempFactLeft (append tempFactLeft (list (nth 0 (nth factsIndex tempFacts)))))
				(dotimes (counter (length (nth 1 (nth factsIndex tempFacts))))
					(if (equal (type-of (nth counter (nth 1 (nth factsIndex tempFacts)))) (type-of 'symbol))
						(progn
							(if (equal (length (string (nth counter (nth 1 (nth factsIndex tempFacts))))) 3)
								(progn
									(setf tempFactRight (append tempFactRight  (list (nth counter (nth 1  query)))))
								)
							)
						)
						(setf tempFactRight (append tempFactRight  (list (nth counter (nth 1 (nth factsIndex tempFacts))))))
					)
				)
				(setf tempFactLeft (append tempFactLeft (list tempFactRight)))
				(if (not (equal nil (position tempFactLeft facts :test #'equal)))
					(setf return_clause_result t)
					(setf return_clause_result nil)
				)
			)
		)
		return_clause_result
	)
)

(defun find_clause (clauses query)
	(let ((returnClause nil))
		(dotimes (clauseIndex (length clauses))
			(let ((clause (nth 0 (nth clauseIndex clauses))) (predicts (nth 1 (nth clauseIndex clauses))))
				(if (equal (nth 0 clause) (nth 0 query))
					(progn
						(if (equal (length (nth 1 clause))  (length (nth 1 query)))
							(progn
								(dotimes (index (length (nth 1 clause)))
									(let ((element_clause (nth index (nth 1 clause))) (element_query (nth index (nth 1 query))))
										(if (equal (type-of element_query) (type-of element_query))
											(progn
												(if (not (equal (type-of element_clause) (type-of 'symbol)))
													(progn
														(if (equal element_clause element_query)
															(setf returnClause (append returnClause (list (nth clauseIndex clauses))))
														)
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)
			)
		)
		returnClause
	)
)

(defun main(filename outputFilename)
	(let ((input_list '()) (outputList '()))
		(setf input_list (func filename outputFilename))
		(setf outputList (function_prolog input_list))
		(with-open-file (stream outputFilename :direction :output )
			(dotimes (n (length outputList))
				(format stream (string (nth n outputList)))
				(terpri stream)
			)
		   (close stream)
		)
	)
)

(main "input.txt" "output.txt")