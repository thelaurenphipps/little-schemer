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

;(define rember
;  (lambda (a lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) a) (cdr lat))
;      (else (cons (car lat)
;                  (rember a
;                          (cdr lat)))))))

(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

;(define insertR
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) (cons old (cons new (cdr lat))) )
;      (else (cons (car lat) (insertR new old (cdr lat)))))))

;(define insertL
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) (cons new (cons old (cdr lat)))) ;this could have been ((eq? (car lat) old) (cons new lat))
;      (else (cons (car lat) (insertL new old (cdr lat)))))))

;(define subst
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) (cons new (cdr lat)))
;      (else (cons (car lat) (subst new old (cdr lat)))))))

;(define subst2
;  (lambda (new o1 o2 lat)
;    (cond
;      ((null? lat) (quote()))
;      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
;      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

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

(define add1
  (lambda (n)
     (+ n 1)))

(define sub1
  (lambda (n)
     (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

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

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((or (> n m) (< n m)) #f)
      (else #t))))

(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (power n (sub1 m))) ))))

(define o/
  (lambda (n m)
    (cond
      ((< n m) 0 )
      (else (add1 (o/ (- n m) m))))))

(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0 )
      (else (+ 1 (olength (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat)) ;why do I subtract 1 again?
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
 (lambda (lat)
   (cond
     ((null? lat) (quote()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ; ((or (number? a1) (number a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick-one
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick-one (sub1 n) (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l)))))
       )
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))



(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2) (eqan? s1 s2)))
      ((or (atom? s1) (atom? s2) #f)) ; we know that they can't both be atoms... only one of them can be
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define numbered?
  (lambda (exp)
    (cond
      ((atom? exp) (number? exp))
      (else (and (numbered? (car exp)) (numbered? (car (cdr (cdr exp)))))))))
                   
(define pre-first-sub-exp
  (lambda (exp)
    (car (cdr exp))))

(define second-sub-exp
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define pre-operator
  (lambda (exp)
    (car exp)))

; rewrite value with these new helper methods
;(define value
;  (lambda (exp)
;    (cond
;      ((atom? exp) exp)
;      ((eq? (pre-operator exp) '+) (+ (pre-first-sub-exp exp) (second-sub-exp exp)))
;      ((eq? (pre-operator exp) '*) (* (pre-first-sub-exp exp) (second-sub-exp exp)))
;      (else (expt (pre-first-sub-exp exp) (second-sub-exp exp))))))

; use this value function for the first representatio of arithmetic expressions
; answer: change the definitions of first-sub-exp and operator to be reversed for inline evaluation
                                        
(define in-first-sub-exp
  (lambda (exp)
    (car exp)))

(define in-operator
  (lambda (exp)
    (car (cdr exp))))

(define in-value
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (in-operator exp) '+) (+ (in-first-sub-exp exp) (second-sub-exp exp)))
      ((eq? (in-operator exp) '*) (* (in-first-sub-exp exp) (second-sub-exp exp)))
      (else (expt (in-first-sub-exp exp) (second-sub-exp exp))))))

(define rep-zero?
  (lambda (exp)
    (null? exp)))

(define rep-add1
  (lambda (exp)
    (cons (quote ()) exp)))

(define rep-sub1
  (lambda (exp)
    (cdr exp)))


(define rep-plus
  (lambda (a b)
    (cond
      ((rep-zero? b) a)
      (else (rep-add1 (rep-plus a (rep-sub1 b)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and  (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (expr)
    (cond
      ((atom? expr) #f) ; can't NOT be a list of things
      ((null? expr) #f)
      ((null? (cdr expr)) #f) ; can't have only one element in the list
      ((null? (cdr (cdr expr))) #t) ; only has two elements
      (else #f))))

(define first-ref
  (lambda (p)
    (car p)))

(define second-ref
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

; --- chapter 8 ---

;(define rember-f
;  (lambda (test? a l)
;    (cond
;      ((null? l) (quote()))
;      ((test? a (car l)) (cdr l))
;      (else (cons (car l) (rember-f test? a (cdr l)))))))

;(rember-f = 5 '(6 2 5 3))
;(rember-f eq? 'jelly '(jelly beans are good))
;(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(eq?-c 'salad)

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'salad)
(eq?-salad 'tuna)

((eq?-c 'salad) 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote()))
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

((rember-f =) 5 '(6 2 5 3))
((rember-f eq?) 'jelly '(jelly beans are good))
((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))


(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) old) (cons new (cons old (cdr lat))))
        (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))


;(define insertR
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) (cons old (cons new (cdr lat))) )
;      (else (cons (car lat) (insertR new old (cdr lat)))))))

;(define insertL
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) (cons new (cons old (cdr lat)))) ;this could have been ((eq? (car lat) old) (cons new lat))
;      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; order is one of (cons old (cons new ...)) and (cons new (cons old ...))
; test? is equal? or eq? or ...
; new is the element to insert, old is the element that triggers the insert, lat is the list of elements to operate on

(define insert-g
  (lambda (order)
      (lambda (new old lat)
        (cond
          ((null? lat) (quote()))
          ((eq? (car lat) old) (order new old (cdr lat)))
          (else (cons (car lat) ((insert-g order) new old (cdr lat))))))))
        
;((insert-g seqL) 1 2 '(1 2 3))


;(define insertL (insert-g seqL))
;(define insertR (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define seqS
  (lambda (new old l)
    (cons new l))) ; l is cdr l when used in the context of insert-g

(define subst(insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(yyy 'sausage '(pizza with sausage and bacon))


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else exp))))

(atom-to-function (pre-operator '(+ 5 3)))


;(define value
;  (lambda (exp)
;    (cond
;      ((atom? exp) exp)
;      ((eq? (pre-operator exp) '+) (+ (pre-first-sub-exp exp) (second-sub-exp exp)))
;      ((eq? (pre-operator exp) '*) (* (pre-first-sub-exp exp) (second-sub-exp exp)))
;      (else (expt (pre-first-sub-exp exp) (second-sub-exp exp))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (pre-operator nexp)) (value (pre-first-sub-exp nexp)) (value (second-sub-exp nexp)))))))

(value '(+ 5 3))


;(define multirember
;  (lambda (a lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) a) (multirember a (cdr lat)))
;      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote()))
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote()) (quote())))
      ((eq? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))


(define a-friend
  (lambda (x y)
    (null? y)))

;(define new-friend
;  (lambda (newlat seen)
;    (col newlat
;         (cons (car lat) seen))))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons (quote tuna) seen))))

(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)
(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '(and test) a-friend)

(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co (quote tuna) '(strawberries tuna and swordfish) last-friend)
(multirember&co '(quote tuna) '(strawberries tuna and swordfish) last-friend)
(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)
(multirember&co '(quote tuna) '(strawberries (quote tuna) and swordfish) last-friend)
(multirember&co '(quote tuna) '(strawberries '(quote tuna) and swordfish) last-friend)

(define multiinsertLR
  (lambda ( new oldL oldR lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda ( new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote()) 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else (cons (car lat) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons (car lat) newlat) L R) )))))))

(define even?
  (lambda (n)
    (= (modulo n 2) 0)))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))


(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* (car l) p ) s))))
         (else (evens-only*&co (cdr l) (lambda (newl p s) (col newl p (+ (car l) s)))))))
       (else (evens-only*&co (car l) (lambda (al ap as) (evens-only*&co (cdr l) (lambda (dl dp ds) (col (cons al dl) (* ap dp) (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

                '(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

'(evens-only*&co '(9 1 2 8) the-last-friend)
(evens-only*&co '(9 1 2 8) the-last-friend)

'(evens-only*&co '((9 1 2 8)) the-last-friend)
(evens-only*&co '((9 1 2 8)) the-last-friend)

'(evens-only*&co '((9 1 2 8) 3) the-last-friend)
(evens-only*&co '((9 1 2 8) 3) the-last-friend)

'(evens-only*&co '((9 1 2 8) 3 10) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10) the-last-friend)

'(evens-only*&co '((9 1 2 8) 3 10 ((9 9))) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10 ((9 9))) the-last-friend)

'(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7)) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7)) the-last-friend)

'(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6)) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6)) the-last-friend)

'(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)