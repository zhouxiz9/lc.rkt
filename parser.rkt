#lang racket

(require "lexer.rkt")
(require "token.rkt")


;; parser


(define-struct abstraction (param body))

(define-struct identifier (value))

(define-struct application (lhs rhs))


(define (build-application lhs rhs)
  (cond ((null? rhs) lhs)
        (else (build-application (make-application lhs (car rhs)) (cdr rhs)))))


(define (parse-term tokens)
  (let-values ([(rest-tokens success) (lexer-skip LAMBDA tokens)])
    (if success
        (let-values ([(lcid after-match-lcid) (lexer-match LCID rest-tokens)])
          (if after-match-lcid
              (let-values ([(rest-tokens success) (lexer-skip DOT after-match-lcid)])
                (if success
                    (let-values ([(term after-parse-term) (parse-term rest-tokens)])
                      (if after-parse-term
                          (values (make-abstraction lcid term) after-parse-term)
                          (values rest-tokens #f)))
                    (values after-match-lcid #f)))
              (values tokens #f)))
        (let-values ([(application after-parse-application) (parse-application rest-tokens)])
          (if after-parse-application
              (values application after-parse-application)
              (values rest-tokens #f))))))


(define (parse-application tokens)
  (let-values ([(atom after-parse-atom) (parse-atom tokens)])
    (if after-parse-atom
        (let-values ([(app-plus after-parse-app-plus) (parse-app-plus after-parse-atom)])
          (if after-parse-app-plus
              (values (build-application atom app-plus) after-parse-app-plus)
              (values atom after-parse-atom)))
        (values tokens #f))))

(define (parse-app-plus tokens)
  (letrec ((parse  (lambda (tokens)
                     (if (null? tokens) '()
                         (let-values ([(atom after-parse-atom) (parse-atom tokens)])
                           (if after-parse-atom
                               (cons atom (parse after-parse-atom))
                               #f))))))
    (let ((results (parse tokens)))
      (if (or (not results) (any false? results))
          (values tokens #f)
          (values results #t)))))

            

(define (parse-atom tokens)
  (if (token-type=? (first tokens) LCID)
      (values (make-identifier (token-value (car tokens))) (cdr tokens))
      (let-values ([(rest-tokens success) (lexer-skip LPAREN tokens)])
        (if success
            (let-values ([(term after-parse-term) (parse-term rest-tokens)])
              (if after-parse-term
                  (let-values ([(rest-tokens success) (lexer-skip RPAREN after-parse-term)])
                    (if success
                        (values term rest-tokens)
                        (values after-parse-term #f)))
                  (values rest-tokens #f)))
            (values tokens #f)))))
      
            

(define (parse tokens)
  (let-values ([(ast success) (parse-term tokens)])
    (if success
        ast
        (error "Syntax error"))))


(define (print-parse-result result)
  (cond ((abstraction? result)
         (print-parse-result (abstraction-param result))
         (print-parse-result (abstraction-body result)))
        ((identifier? result)
         (print (identifier-value result)))
        ((application? result)
         (print (application-lhs result))
         (print (application-rhs result)))
        (else (print result))))




;; helper functions


(define (any pre lst)
  (> (length (filter pre lst)) 0))