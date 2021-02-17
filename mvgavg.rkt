#lang racket
(require test-engine/racket-tests)

(define calcMovingAverage
  (lambda (buildMetrics)
    (cond
      ((null? (cdr (cdr buildMetrics)) '())
      (else (cons (/ (addValues (getValues (takeFirst3 buildMetrics))) 3) (calcMovingAverages (cdr buildMetrics))))
 ))))


(define groupByMonth
  (lambda (buildMetrics)
    (cond
      ((null? buildMetric) '())
      (

(define getValues )

(def addValues
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ (car l) (addValues (cdr l)))))))


(define takeFirst3
  (lambda (buildMetrics)
    (cond
      (

