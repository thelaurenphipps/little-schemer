#lang racket
(require test-engine/racket-tests)

(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

(atom? 'atom)
(atom? 'turkey)
(atom? '1492)
(atom? 'u)
(atom? '*abc$)

(list? '(atom))
(list? '(atom turkey or))
(list? '((atom turkey) or))
(list? '(how are you doing so far))
(list? '(((how) are) ((you) (doing so)) far))
(list? '())
(atom? '())
(list? '(() () () ()))


(car'(a b c))
(car'((a b c) x y z))
(car'(((hotdogs)) (and) (pickle) relish))
(car(car '(((hotdogs)) (and))))

(cdr '(a b c))
(cdr '((a b c) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
(cdr '( (b) (x y) ((c))) )

(car (cdr '( (b) (x y) ((c))) ))
(cdr (cdr '( (b) (x y) ((c))) ))
(car '(a (b (c)) d) )

(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())
(car '((b) c d) )
(cons 'a (car '((b) c d) ))
(cons 'a (cdr '((b) c d)))

(null? '())
(null? (quote())) ;quote is a  notation for the null list
(null? '(a b c))


(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(car '(Harry had a heap of apples))
(atom? (car '(Harry had a heap of apples)))
(cdr '(Harry had a heap of apples))
(atom? (cdr '(Harry had a heap of apples)))
(cdr '(Harry))
(atom? (cdr '(Harry)))
(cdr '(swing (low sweet) cherry oat))
(car (cdr '(swing (low sweet) cherry oat)))
(atom? (car (cdr '(swing (low sweet) cherry oat))))

(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(car '(Mary had a little lamb chop))
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(cdr '(beans beans we need jelly beans))
(car (cdr '(beans beans we need jelly beans)))
(car '(beans beans we need jelly beans))
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))