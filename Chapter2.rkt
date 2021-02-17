#lang racket
(require test-engine/racket-tests)

(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

(define lat? (lambda (l)
(cond
((null? l) #t)
((atom? (car l)) (lat? (cdr l)))
(else #f))))


(lat? '(a))
(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))

(or (null? '()) (atom? '(d e f g)))
(or (null? '(a b c)) (null? '()))
(or (null? '(a b c)) (null? '(atom)))


(define member? (lambda (a lat)
(cond
((null? lat) #f)
(else (or (eq? (car lat) a)
(member? a (cdr lat)))))))


(member? 'meat '(mashed potatoes and meat gravy))
(or (eq? (car '(mashed potatoes and meat gravy)) 'meat) (member? 'meat (cdr '(mashed potatoes and meat gravy))))
(member? 'liver '(bagels and lox))
