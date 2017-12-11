#lang racket

(require "token.rkt")

;; lexer

(provide
 lexer
 lexer-skip
 lexer-match)

(define lexer
  (lambda (e)
    (letrec ((lex (lambda (str)
                    (cond ((non-empty-string? str)
                           (let ((current-char (substring str 0 1)))
                             (match current-char
                               [(pregexp #px"\\s") (lex (substring str 1))]
                               [(or "λ" "\\")
                                (list* (make-token LAMBDA current-char) (lex (substring str 1)))]
                               ["." 
                                (list* (make-token DOT current-char) (lex (substring str 1)))]
                               ["(" 
                                (list* (make-token LPAREN current-char) (lex (substring str 1)))]
                               [")" 
                                (list* (make-token RPAREN current-char) (lex (substring str 1)))]
                               [(pregexp #px"[a-z]")
                                (let ((identifier (regexp-match #px"[a-z]+[a-zA-Z]*\\b" str)))
                                  (if identifier
                                      (list* (make-token LCID (car identifier)) (lex (substring str (string-length (car identifier)))))
                                      (error "Wrong variable name")))]
                               [_ (display str)])))
                          (else '())))))
      (lex e))))

(define (print-tokens tokens)
  (for-each
   (lambda (token)
     (print (token-type token))
     (print (token-value token))) tokens))


(define (lexer-skip type tokens)
  (if (and (not (null? tokens)) (equal? (token-type (car tokens)) type))
      (values (cdr tokens) #t)
      (values tokens #f)))


(define (lexer-match type tokens)
  (if (and (not (null? tokens)) (equal? (token-type (car tokens)) type))
      (values (token-value (car tokens)) (cdr tokens))
      (values tokens #f)))