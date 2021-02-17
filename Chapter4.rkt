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

(define member? (lambda (a lat)
(cond
((null? lat) #f)
(else (or (eq? (car lat) a)
(member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))) )
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cons old (cdr lat)))) ;this could have been ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(atom? '14)

(define add1
  (lambda (n)
     (+ n 1)))

(add1 67)

(define sub1
  (lambda (n)
     (- n 1)))

(sub1 5)
(zero? 0)
(zero? 1492)
(+ 46 12)

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(o+ 46 12)
(- 14 3)
(- 17 9)
(- 18 25) ; but we don't deal with negative numbers... so no answer (zero?)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(addtup '(3 5 2 8))
(addtup '(15 6 7 12 3))

(* 5 3)
(* 13 4)

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (o* n (sub1 m)))))))

(o* 5 3)
(o* 13 4)
(o* 12 3)

;(define tup+
;  (lambda (tup1 tup2)
;    (cond
;      ((and (null? tup1) (null? tup2)) (quote()) )
;      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
;(tup+ '(2 3) '(4 6))
;(tup+ '(3 7) '(4 6))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 7) '(4 6 8 1))
(tup+ '(4 6 8 1) '(3 7))
(tup+ '(3 7) '(4 6))

(> 12 133)
(> 120 11)

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(o> 12 133)
(o> 120 11)

(< 4 6)
(< 8 3)
(< 6 6)

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(o< 4 6)
(o< 8 3)
(o< 6 6)

;(define =
;  (lambda (n m)
;    (cond
;      ((zero? m) (zero? n))
;      ((zero? n) #f)
;      (else (= (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((or (> n m) (< n m)) #f)
      (else #t))))

(o= 4 4)
(o= 5 6)

(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (power n (sub1 m))) ))))

(power 1 1)
(power 2 3)
(power 5 3)

(define o/
  (lambda (n m)
    (cond
      ((< n m) 0 )
      (else (add1 (o/ (- n m) m))))))

(o/ 15 4)

(length '(hotdogs with mustard sauerkraut and pickles))

(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0 )
      (else (+ 1 (olength (cdr lat)))))))

(olength '(hotdogs with mustard sauerkraut and pickles))
(olength '(ham and cheese on rye))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat)) ;why do I subtract 1 again?
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spathetti raviloi macaroni meatball))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

(number? 'tomato)
(number? 76)

(define no-nums
 (lambda (lat)
   (cond
     ((null? lat) (quote()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

; TODO - all-nums

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ; ((or (number? a1) (number a2)) #f)
      (else (eq? a1 a2)))))

(eqan? 'a 43)
(eqan? 43 43)
(eqan? 'a 'a)
(eqan? 42 43)
(eqan? 'a 'b)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(occur 5 '(5 pears 6 prunes 9 dates))
(occur 'rp '(5 pears 6 prunes 9 dates))

(define one?
  (lambda (n)
    (= n 1)))

(one? 1)
(one? 2)

(define rempick-one
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick-one (sub1 n) (cdr lat)))))))

(rempick-one 3 '(hotdogs with hot mustard))
(rempick-one 3 '(lemon meringue salty pie))