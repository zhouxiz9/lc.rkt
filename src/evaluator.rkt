#lang racket

(require test-engine/racket-tests)
(require "token.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define ALPHA "α")
(define BETA "β")

(define REDUCTION-TYPE '())


(define (reduce ast)
  (cond ((need-alpha-reduction? ast)
         (set! REDUCTION-TYPE ALPHA)
         (letrec ((update-id
                   (lambda (old-id)
                     (let ((new-id (gen-new-id old-id)))
                       (if (member new-id (get-ids ast))
                           (update-id new-id)
                           new-id))))
                  (update-ast
                   (lambda (old-ast ast)
                     (cond ((application? old-ast)
                            (make-application (update-ast (application-lhs old-ast) ast)
                                              (update-ast (application-rhs old-ast) ast)))
                           ((abstraction? old-ast)
                            (let ((param (abstraction-param old-ast)))
                              (if (id-duplicate? param ast)
                                  (let ((new-param (make-identifier (update-id (identifier-value param)))))
                                    (make-abstraction new-param (subst (abstraction-body old-ast) param new-param)))
                                  old-ast)))
                           (else old-ast)))))
           (update-ast ast ast)))                             
        (else
         (set! REDUCTION-TYPE BETA)
         (cond ((application? ast)
                (let ((lhs (application-lhs ast))
                      (rhs (application-rhs ast)))
                  (cond ((abstraction? lhs)
                         (subst (abstraction-body lhs) (abstraction-param lhs) rhs))
                        ((application? lhs)
                         (make-application (reduce lhs) rhs))
                        ((application? rhs)
                         (make-application lhs (reduce rhs)))
                        (else ast))))
               ((abstraction? ast)
                (make-abstraction (abstraction-param ast) (reduce (abstraction-body ast))))
               (else ast)))))


(define (subst e v e1)
  (cond ((identifier? e)
         (if (equal-identifier? e v) e1 e))
        ((application? e)
         (make-application (subst (application-lhs e) v e1)
                           (subst (application-rhs e) v e1)))
        ((abstraction? e)
         (let ((param (abstraction-param e))
               (body (abstraction-body e))
               (fve (map identifier-value (freevars e)))
               (fve1 (map identifier-value (freevars e1))))
           (cond ((equal-identifier? param v) e)
                 ((not (member (identifier-value param) fve1))
                  (make-abstraction param (subst body v e1)))
                 (else
                  (letrec ((gen-new-param
                            (lambda (old-param)
                              (let ((new-param (symbol->string (gensym (identifier-value old-param)))))
                                (if (member new-param (list-union fve fve1))
                                    (gen-new-param param)
                                    (let ((tempe (subst body old-param (make-identifier new-param))))
                                      (make-abstraction (make-identifier new-param) (subst tempe v e1))))))))
                    (gen-new-param param)))))) 
        (else e)))


(define (freevars e)
  (cond ((identifier? e) (list e))
        ((application? e)
         (list-union (freevars (application-lhs e)) (freevars (application-rhs e))))
        ((abstraction? e)
         (remove (abstraction-param e) (freevars (abstraction-body e))))
        (else e)))


(define (reducible? node)
  (cond ((identifier? node) #f)
        ((abstraction? node) (reducible? (abstraction-body node)))
        ((application? node) #t)
        (else (error "Unknown ast node type"))))


(define (complete-reduce ast)
  (if (reducible? ast)
      (let ((result (reduce ast)))
        (if (equal? (ans ast) (ans result))
            (display "infinite reduction")
            (complete-reduce result)))
      ast))
  

(define (get-ids ast)
  (cond ((application? ast)
         (append (get-ids (application-lhs ast))
                 (get-ids (application-rhs ast))))
        ((abstraction? ast)
         (append (list (identifier-value (abstraction-param ast)))
                 (get-ids (abstraction-body ast))))
        (else '())))

                                        
(define (gen-new-id old-id)
  (symbol->string (gensym old-id)))

(define (need-alpha-reduction? ast)
  (check-duplicates (get-ids ast)))

(define (id-duplicate? id ast)
  (> (count (lambda (i) (equal? i (identifier-value id))) (get-ids ast)) 1))
                                         
(define (equal-identifier? i1 i2)
  (equal? (identifier-value i1) (identifier-value i2)))


(define (print-reduce-result str)
  (let ((result (reduce (parse (lexer str)))))
    (print (ast-node->string result))))

(define (ast-node->string node)
  (cond ((identifier? node)
         (string-append (identifier-value node)))
        ((abstraction? node)
         (string-append "(λ" (identifier-value (abstraction-param node)) "." (ast-node->string (abstraction-body node)) ")"))
        ((application? node)
         (string-append "(" (ast-node->string (application-lhs node))  " " (ast-node->string (application-rhs node)) ")"))
        (else node)))


(define (reduce-str exp)
  (reduce (parse (lexer exp))))

         
(define (ans exp)
  (let ((result  (ast-node->string (if (string? exp) (reduce-str exp) exp))))
    (string-append (if (equal? REDUCTION-TYPE ALPHA) ALPHA BETA) " > " result)))
      

;; helper functions
(define (list-union l1 l2)
  (remove-duplicates (append l1 l2)))

;; tests


(check-expect (ans "x") (ans (make-identifier "x")))
(check-expect (ans "\\x.x") (ans (make-abstraction (make-identifier "x") (make-identifier "x"))))
(check-expect (ans "(\\x.x) y") (ans (make-identifier "y")))


(test)

;; examples

;; (ans "(\\x.(\\s.(s s))) (\\s.s)")
;; "α > ((λx.(λs.(s s))) (λs215619.s215619))"

;; (ans "(\\x.(\\s.(s z x))) (\\z.(z s))")
;; "β > (λs158871.(s158871 z))"