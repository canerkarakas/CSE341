(defun primecrawler (filename)
    (let ((upBound nil) (lowBound nil) (line nil) (temp '()) (size 0) (go 0))
    	(setf line (nth 0 (read-file-as-lines filename)))
    	(dotimes (i (length line))
    		(if (char= (char line i) #\Space)
    			(progn
    				(setf lowBound (listToInt temp size))
    				(setf size 0)
    				(setf temp '())
    				(setf go 1)
    			)
    			(progn
    				(setf temp (append temp (list (char line i))))
    				(setf size (+ size 1))
    			)
    		)
    	)
    	(setf upBound (listToInt temp size))
    	(if (= lowBound 1)
    		(setf lowBound 2))
    	(findSolution lowBound upBound)
    )
)

(defun findSolution(lowBound upBound)
	(let ((primeValue nil)(str nil))
		(with-open-file(stream "primedistribution.txt" :direction :output )
			(loop for x from lowBound to upBound
				do(progn
					(setf primeValue (primeOrSemiPrime x))
					(setf x (write-to-string x))
					(if (= 0 primeValue)
						(progn
							(setf str (concatenate 'string x " is Prime"))
							(format stream str)
							(terpri stream)
						)
						(progn
							(if (not (=  primeValue -1))
								(progn
									(setf str (concatenate 'string x " is Semi-Prime"))
									(format stream str)
									(terpri stream)
								)
							)
						)
					)
					(setf x (parse-integer x))
				)
			)
		)
	)
)

(defun isPrime(number index prime)
	(if (= number index)
		prime
		(progn
			(if (= 0 (mod number index))
				(isPrime number (+ 1 index) (+ 1 prime))
				(isPrime number (+ 1 index) prime)
			)
		)
	)
)

(defun primeOrSemiPrime(number)
	(if (= 0 (isPrime number 2 0))
		0
		(isSemiPrime number 2 '())
	)
)

(defun isSemiPrime(number index lst)
	(if (= number index)
		(progn
			(let ((control 0))
				(setf control (controlSemiPrimeList lst control))
				(if (= control 0)
					2
					-1
				)
			)
		)
		(progn
			(if (= 0 (mod number index))
				(progn
					(if (> (length lst) 2)
						-1
						(progn
							(setf lst (append lst (list index)))
							(isSemiPrime number (+ 1 index) lst)
						)
					)
				)
				(isSemiPrime number (+ 1 index) lst)
			)
		)
	)
)

(defun controlSemiPrimeList(lst control)
	(if (null lst)
		control
		(progn
			(if (= (isPrime (nth 0 lst) 2 0) 0)
				(controlSemiPrimeList (cdr lst) control)
				-1
			)
		)
	)
)

(defun listToInt(lst size)
	(let ((result 0))
		(dolist (i lst)
			(setf size (- size 1))
			(setf result (+ result (* (c2i i) (expt 10 size))))
		)
		result
	)
)

(defun read-file-as-lines (filename)
	(with-open-file (in filename)
		(loop for line = (read-line in nil nil)
			while line
				collect line)
	)
)

(defun c2i (x)
	(- (char-int x) (char-int #\0))
)

(primecrawler "boundries.txt")