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
;      ((null? lat) (quote ()))
;      (else (cond
;              ((eq? (car lat) a) (cdr lat))
;              (else (rember a
;                            (cdr lat))))))))
;
;(rember 'bacon '(bacon lettuce and tomato))
;(rember 'and '(bacon lettuce and tomato))

;(define rember
;  (lambda (a lat)
;    (cond
;      ((null? lat) (quote()))
;      (else (cond
;              ((eq? (car lat) a) (cdr lat))
;              (else (cons (car lat)
;                          (rember a
;                                    (cdr lat)))))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))

(rember 'and '(bacon lettuce and tomato))
(rember 'and '(and and and))
(rember 'sauce '(soy sauce and tomato sauce))


(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)))
(firsts '((a b) (c d)))
(firsts '((five plums) (four) (eleven green oranges)))
(firsts '(((five plums) four) (eleven green oranges) ((no) more)))
(rember 'sauce '(soy sauce and tomato sauce))

;(define insertR
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) cons (new (cdr lat)))
;      (else (cons (car lat) (cdr lat))))))

;(define insertR
;  (lambda (new prompt lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? car(lat) prompt) cons( prompt (cons (new cdr(lat))) ))
;      (else cons (car(lat) cdr(lat))) )))

;(define insertR
;  (lambda(new prompt lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) prompt) (cons prompt (cons new (cdr lat))))
;      (else  (cons (car lat) (cdr lat))))))

;(define insertR
;  (lambda (new prompt lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car (cdr lat)) prompt) (cons new (cdr lat)))
;      (else cons (car lat) (insertR new prompt (cdr lat))))))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))) )
      (else (cons (car lat) (insertR new old (cdr lat)))))))

 
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(taco tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cons old (cdr lat)))) ;this could have been ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'topping 'fudge '(ice cream with fudge for dessert))

;(define subst
;  (lambda (new old lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) old) (cons new (subst new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'topping 'fudge '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
(subst2 'vanilla 'chocolate 'banana '(some ice cream with chocolate topping))
(subst2 'vanilla 'chocolate 'banana '(some ice cream with sprinkle topping))

;(define multirember
;  (lambda (a lat)
;    (cond
;      ((null? lat) (quote()))
;      ((eq? (car lat) a) (multirember a lat))
;      (else (cons (car lat) (multirember a lat))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'cup '(coffee cup tea cup and hick cup))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(multiinsertR 'test 'prompt '(testing prompt))
(multiinsertR 'test 'prompt '(prompt testing prompt))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'fried 'fish '(chips and fish or fish and fried))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst 'old 'new '(the old game was played on the old table))
(multisubst 'new 'old '(the old game was played on the old table))