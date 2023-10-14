;Chad Huntebrinker
;3/4/2022
;CS 441

#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;This is the scanner/lexer which changes our program into tokens to make sure the program is valid.
(define scanner
  (lexer

   ;Take care of the known keywords (if read, and write)
   ["if" (cons '"if" (scanner input-port))]
   ["read" (cons '"read"(scanner input-port))]
   ["write" (cons '"write"(scanner input-port))]

   ;Take care of the brackets and parentheses
   [#\( (cons '"open-parentheses" (scanner input-port))]
   [#\) (cons '"closed-parentheses" (scanner input-port))]
   [#\{ (cons '"open-bracket" (scanner input-port))]
   [#\} (cons '"closed-bracket" (scanner input-port))]

   ;Take care of the add_ops (+ and -) and the assignment operator (=)
   [(:or #\+ #\-) (cons `"add_op"(scanner input-port))]
   [#\= (cons `"assign_op"(scanner input-port))]
   
   ;Take care of ids, which can be a single character or a string
   [(:+ (:or (char-range #\a #\z)(char-range #\A #\Z)))
    (cons `"id" (scanner input-port))]

   ;Take care of digits, which can be one number or multiple numbers
   [(:+ (char-range #\0 #\9))
    (cons `"digit" (scanner input-port))]  

   ;Semicolons will be replaced to the string "semi-colon"
   [#\; (cons `"semi-colon" (scanner input-port))]

   ;If there's a tab, a whitespace, or a return we want to get rid of it
   [(:or #\return #\tab #\space) (scanner input-port)]

   ;If it's a newline, we want to keep it so that if there is a parse error we know what line it is on
   [#\newline (cons `"new-line" (scanner input-port))]

   ;Take care of the end symbol "$$"
   ["$$" (cons '"$$" (scanner input-port))]

   ;If we reach the end of the file, add EOF and don't scan anymore
   [(eof) '("EOF")]

   ;If there are any characters that don't meet the above cirteria, it is an illegal character and stop scanning.
   [any-char (list "Illegal")]

   ))

;This is our parse function that starts the whole parser program.
(define (parse input_file)

  ;Make our token list and orig_token_list (orig_token_list will only be used if there is a parse error).
  (define token_list (scanner(open-input-file input_file)))
  (define orig_token_list token_list)

  ;Call program
  (program token_list orig_token_list))

;This is the program function that checks to see if it is a program.
;The token_list is the list the parser uses throughout the process.
;The orig_token_list is only used if there is a parse error to find what line it is on.
(define (program token_list orig_token_list)
  (cond
    
    ;If the last token that was scanned is illegal, then print that it is illegal.
    [(equal? "Illegal" (last token_list))
     (printf "Illegal Character")]

    #|
     If the first token is an open bracket, call the open-bracket function and return a new list.
     Then, take the new_list and call the stmt_list function (which will return a new2_list).
     Then, take the new2_list and call the program function again.
    |#
    [(equal? "open-bracket" (first token_list))
     (define new_token_list (open-bracket token_list))
     (define new2_token_list(stmt_list new_token_list))
     (program new2_token_list orig_token_list)]


    #|
     If the first token is a stmt_list, call the stmt_list function and return a new_list.
     Then, take the new_list and call the program function.
    |#
 [(or (equal? "id" (first token_list)) (equal? "if" (first token_list)) (equal? "read" (first token_list)) (equal? "write" (first token_list)) (equal? "closed-bracket" (first token_list)))
     (define new_token_list (stmt_list token_list))
     (program new_token_list orig_token_list)]

    #|
     If the first token is a $$, call the $$ function and return a new_list.
     Then, take the new_list and call the program function.
    |#
  [(equal? "$$" (first token_list))
     (define new_token_list ($$ token_list))
     (program new_token_list orig_token_list)]

    #|
     If the first token is a new-line, make a new_list without the new-line token (ignore it).
     Then, take the new_list and call the program function.
    |#
  [(equal? "new-line" (first token_list))
     (define new_token_list (rest token_list))
     (program new_token_list orig_token_list)]

    #|
     If the first token is an EOF (End Of File), That means everything was parsed correctly.
     So, print Accept.
    |#
  [(equal? "EOF" (first token_list))
     (printf "Accept")]

    #|
     If there is a parse error (whether nothing qualifies in the program function or "stop" is at the top of the list),
     then call the parse_error_message function that will handle that.
    |#
  [else (parse_error_message token_list orig_token_list)]

  ))

;This is the stmt_list function.
(define (stmt_list token_list)
  (cond

    
    ;If the first token is a stmt, call the stmt function.
  [(or (equal? "id" (first token_list)) (equal? "if" (first token_list)) (equal? "read" (first token_list)) (equal? "write" (first token_list)))
     (stmt token_list)]

    ;If the first token is a closed-bracket, call the closed-bracket function.
  [(equal? "closed-bracket" (first token_list))
   (closed-bracket token_list)]

    #|
     If the first token is new-line, ignore it by using just the rest of the list and call the stmt_list function again.
     Then, with the new2_token_list created by the stmt_list function, return the new2_token_list.
    |#
  [(equal? "new-line" (first token_list))
     (define new_token_list (rest token_list))
     (define new2_token_list(stmt_list new_token_list))
     new2_token_list]

    ;If the first token is stop (parse error was detected), return the token_list.  Program will handle it.
  [(equal? "stop" (first token_list))
   token_list]

    ;If the nothing matches, call the parse_error function (which will put "stop" at the top of the list).
  [else (parse_error token_list)]
  
  ))

;This is the stmt function.
(define (stmt token_list)
(cond

    ;If the first token is an id, make sure the rest of the stmt follows the correct grammar (id = expr ;).
  [(equal? "id" (first token_list))
   (define new_token_list(id token_list))
   (define new2_token_list(assign_op new_token_list))
   (define new3_token_list (expr new2_token_list))
   (semi-colon new3_token_list)]

    ;If the first token is an if, make sure the rest of the stmt follows the correct grammar (if (expr) stmt).
  [(equal? "if" (first token_list))
   (define new_token_list(match "if" token_list))
   (define new2_token_list(open-parentheses new_token_list))
   (define new3_token_list(expr new2_token_list))
   (define new4_token_list(closed-parentheses new3_token_list))
   (stmt new4_token_list)]

    ;If the first token is an read, make sure the rest of the stmt follows the correct grammar (read id ;).
  [(equal? "read" (first token_list))
   (define new_token_list(read token_list))
   (define new2_token_list(id new_token_list))
   (semi-colon new2_token_list)]

    ;If the first token is an write, make sure the rest of the stmt follows the correct grammar (write expr ;).
  [(equal? "write" (first token_list))
   (define new_token_list(match "write" token_list))
   (define new2_token_list(expr new_token_list))
   (semi-colon new2_token_list)]

    #|
     If the first token is new-line, ignore it by using just the rest of the list and call the stmt function again.
     Then, with the new2_token_list created by the stmt function, return the new2_token_list.
    |#
  [(equal? "new-line" (first token_list))
     (define new_token_list (rest token_list))
     (define new2_token_list(stmt new_token_list))
     new2_token_list]

    ;If the first token is stop (parse error was detected), return the token_list.  Program will handle it.
  [(equal? "stop" (first token_list))
   token_list]

    ;If the nothing matches, call the parse_error function (which will put "stop" at the top of the list).
  [else (parse_error token_list)]

  ))


;This is the expr function.
(define (expr token_list)
(cond

    #|
     If the first token is an id, call the id function.
     Then, call the etail function.
    |#
  [(equal? "id" (first token_list))
   (define new_token_list(id token_list))
   (etail new_token_list)]

    ;If the first token is a num, call the num function.
  [(or (equal? "digit" (first token_list)) (equal? "add_op" (first token_list)))
   (num token_list)]

    ;If the first token is a semi-colon, call the semi-colon function.
  [(equal? "semi-colon" (first token_list))
   (semi-colon token_list)]

    #|
     If the first token is new-line, ignore it by using just the rest of the list and call the expr function again.
     Then, with the new2_token_list created by the expr function, return the new2_token_list.
    |#
  [(equal? "new-line" (first token_list))
     (define new_token_list (rest token_list))
     (define new2_token_list(expr new_token_list))
     new2_token_list]

    ;If the first token is stop (parse error was detected), return the token_list.  Program will handle it.
  [(equal? "stop" (first token_list))
   token_list]

    ;If the nothing matches, call the parse_error function (which will put "stop" at the top of the list).
  [else (parse_error token_list)]

  ))

;This is the etail function.
(define (etail token_list)
(cond

  #|
   If the first token is an add_op, then call the add_op function.
   Then call the expr function.
  |#
  [(equal? "add_op" (first token_list))
   (define new_token_list(add_op token_list))
   (expr new_token_list)]

  #|
   If the first token a semi-colon or a end-parentheses, then return the list.
   We'll let the stmt function handle that.
  |#
  [(or (equal? "semi-colon" (first token_list)) (equal? "end-parentheses" (first token_list)))
   token_list]

    #|
     If the first token is new-line, ignore it by using just the rest of the list and call the etail function again.
     Then, with the new2_token_list created by the etail function, return the new2_token_list.
    |#
  [(equal? "new-line" (first token_list))
     (define new_token_list (rest token_list))
     (define new2_token_list(etail new_token_list))
     new2_token_list]
  
    ;If the first token is stop (parse error was detected), return the token_list.  Program will handle it.
  [(equal? "stop" (first token_list))
   token_list]

    ;If the nothing matches, call the parse_error function (which will put "stop" at the top of the list).
  [else (parse_error token_list)]

  ))

;This is the num function.
(define (num token_list)
(cond

  #|
   If the first token is an add_op, then call the add_op function.
   Then call the num function.
  |#
  [(equal? "add_op" (first token_list))
   (define new_token_list(add_op token_list))
   (num new_token_list)]

  #|
   If the first token is a digit, then call the digit function.
   Then, return the new_list that the digit function created.
  |#
  [(equal? "digit" (first token_list))
   (define new_token_list(digit token_list))
   new_token_list]

    #|
     If the first token is new-line, ignore it by using just the rest of the list and call the num function again.
     Then, with the new2_token_list created by the num function, return the new2_token_list.
    |#
  [(equal? "new-line" (first token_list))
     (define new_token_list (rest token_list))
     (define new2_token_list(num new_token_list))
     new2_token_list]

    ;If the first token is stop (parse error was detected), return the token_list.  Program will handle it.
  [(equal? "stop" (first token_list))
   token_list]

    ;If the nothing matches, call the parse_error function (which will put "stop" at the top of the list).
  [else (parse_error token_list)]
  
  ))

;The functions below are similar in format.
;If the expected token and the actual token match, then return the list minus the first token.
;Otherwise, it is a parse error.
;In that case, call the parse_error function which will put a "stop" at the front of the list.
;That way, Program can handle it.
(define (read token_list)
(cond 
  [(equal? "read" (first token_list))
     (match "read" token_list)]

[ else
   (parse_error token_list)]

  ))

(define (semi-colon token_list)
(cond
  [(equal? "semi-colon" (first token_list))
     (match "semi-colon" token_list)]

[else (parse_error token_list)]

  ))

(define (write token_list)
(cond
  [(equal? "write" (first token_list))
     (match "write" token_list)]

[else (parse_error token_list)]

  ))

(define (id token_list)
(cond
  [(equal? "id" (first token_list))
     (match "id" token_list)]

[else (parse_error token_list)]

  ))

(define (assign_op token_list)
(cond
  [(equal? "assign_op" (first token_list))
     (match "assign_op" token_list)]

[else (parse_error token_list)]

  ))

(define (digit token_list)
(cond
  [(equal? "digit" (first token_list))
     (match "digit" token_list)]
  
  [else (parse_error token_list)]

  ))


(define (add_op token_list)
(cond
  [(equal? "add_op" (first token_list))
     (match "add_op" token_list)]
  
  [else (parse_error token_list)]

  ))

(define ($$ token_list)
  (cond
[(equal? "$$" (first token_list))
     (match "$$" token_list)]

  [else (parse_error token_list)]

  ))

(define (open-bracket token_list)
(cond
  [(equal? "open-bracket" (first token_list))
     (match "open-bracket" token_list)]

  [else (parse_error token_list)]

  ))


(define (closed-bracket token_list)
(cond
  [(equal? "closed-bracket" (first token_list))
     (match "closed-bracket" token_list)]
  
  [else (parse_error token_list)]

  ))

(define (open-parentheses token_list)
(cond
  [(equal? "open-parentheses" (first token_list))
     (match "open-parentheses" token_list)]
  
[else (parse_error token_list)]

  ))


(define (closed-parentheses token_list)
  (cond
    [(equal? "closed-parentheses" (first token_list))
     (match "closed-parentheses" token_list)]
    
[else (parse_error token_list)]

  ))

#|
  This is the match_function, which is used for all the tokens we converted in the scanner (id, expr, $$, read, (, ], etc.).
  It is not used for some of the broader terms like program, stmt_list, stmt, and num.
|#
(define (match expected token_list)
  (cond

    ;If the expected value matches the first value in the list, return the rest of the list.
    [ (equal? expected (first token_list))
          (rest token_list)]

    ;else it is a parse error, so call the parse_error function.
    [else (parse_error token_list)]

  ))

;If there is a parse error, put a stop at the front of the function.
;Program will handle it.
(define (parse_error token_list)
(cons "stop" token_list))


#|
  If there is a parse error, we want to calculate what line it is on.
  So call our calc_line_number to calculate it on token_list (the list used in the parser) and orig_token_list (the original token list we had after the scanner).
  We will then take: (the original number of new-lines) - (the number of new-lines we found) + 1
  We add 1 because we want to know the line number the error is on, not the number of new-lines the parse has passed.
  Then, out put that information.
|#
(define (parse_error_message token_list orig_token_list)
  (define token_line_number (calc_line_number token_list 0))
  (define orig_token_line_number (calc_line_number orig_token_list 0))
  (define line_number (- orig_token_line_number token_line_number)) 
  (define actual_line_number (+ line_number 1))
  
  (printf "Syntax error on line ")
  (display actual_line_number))

;This function calculates the number of new-lines there are in a list.
(define (calc_line_number token_list line_num)
  (cond

    ;If we found a new line, add one to our line_num (which keeps track of the number of new-lines found).
    [(equal? "new-line" (first token_list))
            (define new_line_num (calc_line_number (rest token_list) (+ 1 line_num)))
            new_line_num]

    ;If we reached the end of the list, return line_num.
    [(equal? "EOF" (first token_list))
            line_num]

    ;If it is not a new-line but we still have more in the list, call the calc_line_num function again with the rest of the list.
    ;After that, return the new_line_num.
    [else
            (define new_line_num(calc_line_number (rest token_list) line_num))
            new_line_num]

    ))