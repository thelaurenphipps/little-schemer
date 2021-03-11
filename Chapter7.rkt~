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
(define value
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (pre-operator exp) '+) (+ (pre-first-sub-exp exp) (second-sub-exp exp)))
      ((eq? (pre-operator exp) '*) (* (pre-first-sub-exp exp) (second-sub-exp exp)))
      (else (expt (pre-first-sub-exp exp) (second-sub-exp exp))))))

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



; --- chapter 7 ---

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '(apple peaches pears plum))
(set? '(apple 3 pear 4 9 apple 3 4))

;(define makeset
;  (lambda (lat)
;    (cond
;      ((null? lat) (quote ()))
;      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
;      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))

;(define subset?
;  (lambda (set1 set2)
;    (cond
;      ((null? set1) #t)
;      ((member? (car set1) set2) (subset? (cdr set1) set2))
;      (else #f))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))


;(define eqset?
;  (lambda (set1 set2)
;    (cond
;      ((subset? set1 set2) (subset? set2 set1))
;      (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and  (subset? set1 set2) (subset? set2 set1))))

(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

;(define intersect?
;  (lambda (set1 set2)
;    (cond
;      ((null? set1) #f)
;      ((member? (car set1) set2) #t)
;      (else (intersect (cdr set1) set2)))))

; left off at top of 115

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
(union '(macaroni and cheese) '(stewed tomatoes and macaroni casserole))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(difference '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
(difference '(macaroni and cheese) '(stewed tomatoes and macaroni casserole))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with some apples)))

(define a-pair?
  (lambda (expr)
    (cond
      ((atom? expr) #f) ; can't NOT be a list of things
      ((null? expr) #f)
      ((null? (cdr expr)) #f) ; can't have only one element in the list
      ((null? (cdr (cdr expr))) #t) ; only has two elements
      (else #f))))

(a-pair? '(pear pear))
(a-pair? '(pear pear pear))
(a-pair? '((2) (pair)))
(a-pair? '(full (house)))

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

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

;(define revrel
;  (lambda (rel)
;    (cond
;      ((null? rel) (quote()))
;      (else (cons (cons (second-ref (car rel)) (cons (first-ref (car rel)) (quote()))) (revrel (cdr rel)))))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (build (second-ref (car rel)) (first-ref (car rel))) (revrel (cdr rel)))))))

 (revrel '((d 4) (b 0) (b 9) (e 5) (g 4)))

; stopped 120