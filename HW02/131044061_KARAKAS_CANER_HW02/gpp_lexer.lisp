; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Caner Karakas                    *
; *********************************************

(setq operators '())
(setq operators (append operators (list (code-char 43.))))
(setq operators (append operators (list (code-char 45.))))
(setq operators (append operators (list (code-char 47.))))
(setq operators (append operators (list (code-char 42.))))
(setq operators (append operators (list (code-char 40.))))
(setq operators (append operators (list (code-char 41.))))
(setq operators (append operators (list (code-char 34.))))
(setq operators (append operators (list (code-char 34.))))
(setq operators (append operators (list (code-char 44.))))

(setf keywords '((#\a #\n #\d) (#\o #\r) (#\n #\o #\t) (#\e #\q #\u #\a #\l)
               (#\l #\e #\s #\s) (#\n #\i #\l) (#\l #\i #\s #\t)
               (#\a #\p #\p #\e #\n #\d) (#\c #\o #\n #\c #\a #\t)
               (#\s #\e #\t) (#\d #\e #\f #\f #\u #\n) (#\f #\o #\r)
               (#\i #\f) (#\e #\x #\i #\t) (#\l #\o #\a #\d)
               (#\d #\i #\s #\p) (#\t #\r #\u #\e) (#\f #\a #\l #\s #\e)))

(setq keywordsTokens '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS"
                     "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET"
                     "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD"
                     "KW_DISP" "KW_TRUE" "KW_FALSE"))

(setq operatorsTokens '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"
                     "OP_OC" "OP_CC" "OP_COMMA" "OP_DBLMULT")) ;-1 unutma OP_DBLMULT yüzünden

(setq comValIDTokens '("COMMENT" "VALUE" "IDENTIFIER"))

(setq integers '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(setq tokens '())

(defun toList (line index result)
   (if (or (equal (nth index line) #\Space) (equal (nth index line) nil))
      (return-from toList result)
   )
   (setf result (append result (list (nth index line))))
   (toList line (+ index 1) result)
)

(defun toListForIntegers (line index result)
   (if (or (equal (nth index line) #\Space) (equal (nth index line) nil))
      (return-from toListForIntegers result)
   )
   (if (equal (member (nth index line) integers) nil)
      (return-from toListForIntegers nil)
   )
   (setf result (append result (list (nth index line))))
   (toListForIntegers line (+ index 1) result)
)

;if has syntax error because of integers, find space index for jump
(defun goSpace (line index result)
   (if (or (equal (nth index line) #\Space) (equal (nth index line) nil))
      (return-from goSpace result)
   )
   (setf result (+ result 1))
   (goSpace line (+ index 1) result)
)

(defun isKeyword (word)
   (let ((control))
      (dotimes (n (length keywords))
         (setf control T)
         (if (equal (length word) (length (nth n keywords)))
            (progn
               (dotimes (m (length word))
                  (if (not (equal (nth m word) (nth m (nth n keywords))))
                     (progn
                        (setf control nil)
                        (setf m (+ m (length word)))
                     )
                  )
               )
               (if (equal control T)
                  (return-from isKeyword control)
               )
            )
         )
         (setf control nil)
      )
      (return-from isKeyword control)
   )
)

(defun indexOfKeyword (word)
   (let ((control))
      (dotimes (n (length keywords))
         (setf control T)
         (if (equal (length word) (length (nth n keywords)))
            (progn
               (dotimes (m (length word))
                  (if (not (equal (nth m word) (nth m (nth n keywords))))
                     (progn
                        (setf control nil)
                        (setf m (+ m (length word)))
                     )
                  )
               )
               (if (equal control T)
                  (return-from indexOfKeyword n)
               )
            )
         )
         (setf control nil)
      )
   )
)

(defun lexer(line)
   (let* ((word '()) (integerVal '()) (control nil) (control2 nil) (control3 nil))
      (dotimes (n (length line))
         (setf word '())
         (setf integerVal '())
         (setf control nil)

         ;comments
         (if (and (equal (nth n line) #\;) (equal (nth (+ n 1) line) #\;))
            (progn 
               (setq tokens (append tokens (list (nth 0 comValIDTokens))))
               (setf control T)
               (setf n (+ n (length line)))
            )
         )

         ;operators
         (if (not (equal (member (nth n line) operators) nil))
            (progn
               (if (and (equal (nth n line) (code-char 42.)) (equal (nth (+ n 1) line) (code-char 42.))) ;** operator icin ozel kontrol
                  (progn
                     (setq tokens (append tokens (list (nth 9 operatorsTokens))))
                     (setf n (+ n 1))
                     (setf control T)
                  )
                  (progn
                     (setq tokens (append tokens (list (nth (- (- (length operatorsTokens) (length (member (nth n line) operators))) 1) operatorsTokens))))
                     (setf control T)
                  )
               )
            )
         )

         ;integers
         (if (not (equal (member (nth n line) integers) nil))
            (progn
               (setf integerVal (toListForIntegers line n integerVal))
               (if (equal integerVal nil)
                  (progn
                     (setf n (+ n (goSpace line n 0)))
                     (setf control T)
                     (setq tokens (append tokens (list "SYNTAX ERROR")))
                  )
                  (progn
                     (setf n (+ n (length integerVal)))
                     (setf control T)
                     (setq tokens (append tokens (list (nth 1 comValIDTokens))))
                  )
               )
            )
         )

         ;id or keyword
         (if (and (equal control nil) (not (equal (nth n line) #\Space)))
            (progn
               (if (not (equal (member (nth n line) alphabet) nil))
                  (progn
                     (setf word (toList line n word))
                     (setf n (+ n (length word)))
                     (if (isKeyword word)
                        (setq tokens (append tokens (list (nth (indexOfKeyword word) keywordsTokens))))
                        (setq tokens (append tokens (list (nth 2 comValIDTokens))))
                     )
                  )
                  (setq tokens (append tokens (list "SYNTAX ERROR")))
               )
            )
         )
      )
   )
)

(defun gppinterpreter (filename)

	(let ((f (open filename :if-does-not-exist nil)))
		(let* ((line '()))
         (when f
            (loop for ch = (read-char f nil)
               while ch do
                  (cond 
                     ((char= ch #\newline)
                        (cond 
                           ((not (equal line nil))
                              (lexer line)
                           )
                        )
                        (setf line '())
                     )
                     ((char= ch (code-char 41.))
                        (setf line (append line (list #\Space)))
                        (setf line (append line (list ch)))
                     )
                     ((char= ch (code-char 40.))
                        (setf line (append line (list ch)))
                        (setf line (append line (list #\Space)))
                     )
                     (t (setf line (append line (list ch))))
                  )
            )
            (close f)
         )
         (lexer line) ;isleme girmeyen son satiri islemek icin
         (return-from gppinterpreter tokens)
      )
	)
)

(defun c2i (x)
	; Convert character to int.
	(- (char-int x) (char-int #\a))
)

(defun i2c (x)
	; Convert int to character.
	(int-char (+ x (char-int #\a)))
)

(gppinterpreter "g++.txt")
(dotimes (n (length tokens))
   (print (nth n tokens))
)