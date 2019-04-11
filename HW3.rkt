#lang racket

(define (E1 n)
 (values
  (display "Area and Perimeter: \n")
  (*(/(*(sqrt 3)(expt n 2) )2)3)
  (* 6 n)
  )
 )

(define (E2 n)
  (cond [(and (> n -1)(integer? n ))
    (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (true (+ (E2 (- n 1)) (E2 (- n 2))))
    )]
    [else (error "ERROR : Wrong input")]    
  ))

(define (E3 list)
  (cond
   ((< (length(filter odd? list )) (length(filter even? list))) "TRUE" )
   (else "FALSE")
  )
 )

(define (E4 sublist list)
  (cond [(and (list? sublist) (list? list))
         (cond
           ((subset? sublist list ) "TRUE")
           (else "FALSE")
           )]
        [else (error "ERROR : Wrong input")]
        )
  )

(define E5
  (lambda (list1 list2)
    (cond ((null? list1)  '())
          ((member (car list1) list2)
           (cons (car list1)
                 (E5 (cdr list1) list2)))
          (else (E5 (cdr list1) list2)))))

(define (E6 lst)
    (if (= (length lst) 1)
        '()
        (append (E6 (cdr lst)) (list (car lst)))))


(define (E7 list1)
    (cond
      ((null? list1) '())
      ((= (car list1) (apply max list1))
          (cons (car list1) (E7 (cdr list1))))
          (else (E7 (append (cdr list1) (list (car list1))))
      )))

(define (E8 lst)
  (if (not (pair? lst))
      0
      (max (add1 (E8 (car lst)))
           (E8 (cdr lst)))))

(define (E9 list1 list2)
  (cond ((equal? (structure list1)
          (structure list2))
       (display "EQUAL"))
  (else (display "NOT EQUAL"  ))))

(define E10
  (lambda (lst)
    (cond ((null? lst)  '())
          ((negative? (car lst))
           (E10 (cdr lst)))
          (else (cons (car lst)(E10 (cdr lst)) )))))

(define (structure lst)
  (if (null? lst)
      lst
      (if (list? (car lst))
          (cons (structure (car lst)) (structure (cdr lst)))
          (structure (cdr lst)))))



  