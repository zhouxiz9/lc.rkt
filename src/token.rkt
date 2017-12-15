#lang racket

(provide
 make-token
 token-type
 token-value
 token-type=?
 LCID
 LAMBDA
 LPAREN
 RPAREN
 DOT
 CONSTANT)

;; Token

(define-struct token (type value))

(define LCID "LCID")
(define LAMBDA "LAMBDA")
(define LPAREN "LPAREN")
(define RPAREN "RPAREN")
(define DOT "DOT")
(define CONSTANT "CONSTANT")


(define (token-type=? token type)
  (equal? (token-type token) type))