; The little Schemer


'atom ; is an atom


(car '(((hotdogs)) (and) (pickle) (relish)) )



(define atom?
  (lambda (x)
    (and (not (pair? x) ) (not (null? x)))))

   
 (define lat?
   (lambda (l)
     (cond ((null? l) #t)
           ((atom? (car l)) (lat? (cdr l)) )
           (else #f))))    
;;;;  remove multi members 
 
 (define rember
   (lambda ( a lat)
     (cond ((null? lat ) '())
           ((eq? (car lat) a ) (rember a (cdr lat) ))
           (else (cons (car lat) (rember a (cdr lat)))))))
 
;;;;; remove only the first occurence 
 (define rember
   (lambda ( a lat)
     (cond ((null? lat ) '())
           ((eq? (car lat) a ) (cdr lat) )
           (else (cons (car lat) (rember a (cdr lat)))))))
 
 
 (define first 
   (lambda (lst) 
     (cond ((null? lst ) '())
           ((pair? (car lst)) ( cons (car (car lst)) (first (cdr lst))))
           (else (first (cdr lst))))))
 
  
 (define first 
   (lambda (lst) 
     (cond ((null? lst ) '())
           ((atom? (car lst)) (first (cdr lst))) 
           (else (cons (car (car lst)) (first (cdr lst)))))))
           
;;;;;  next solution is the one on the book , however it gives error when the first element in lst is not a pair
 (define first 
   (lambda (lst) 
     (cond ((null? lst ) '())
           (else ( cons (car (car lst)) (first (cdr lst)))))))
           
           
           
;;;; insert new to the right of old in lst


(define insertR
  (lambda (new old lst)
    (cond ((null? lst)  '())
          ((eq? (car lst) old ) (cons (car lst) (cons new (cdr lst) )))
          (else (cons (car lst) (insertR new old (cdr lst)))))))
    


;;;; follow the book , every cond clause has only one true branch and one else

(define insertR
  (lambda (new old lst)
    (cond 
     ((null? lst)  '())
     (else (cond
            ((eq? (car lst) old ) 
             (cons old 
                   (cons new (cdr lst) )))
            (else (cons (car lst) 
                        (insertR new old 
                                 (cdr lst)))))))))
    


(define insertL
  (lambda (new old lst)
    (cond 
     ((null? lst)  '())
     (else (cond
            ((eq? (car lst) old ) 
             (cons new 
                   (cons old (cdr lst) )))
            (else (cons (car lst) 
                        (insertL new old 
                                 (cdr lst)))))))))




(define subst
  (lambda (new old lst)
    (cond 
     ((null? lst)  '())
     (else (cond
            ((eq? (car lst) old ) 
             (cons new 
                   (cdr lst) ))
            (else (cons (car lst) 
                        (subst new old 
                               (cdr lst)))))))))


(define rember* 
  (lambda ( a lst)
    (cond ((null? lst) (quote ()))
          ((atom? (car lst))
           (cond ((eq? (car lst) a) 
                  (rember* a (cdr lst)) )
                 (else (cons (car lst) 
                             (rember* a (cdr lst))))))
          (else (cons (rember*  a (car lst)) 
                      (rember* a (cdr lst)))))))


(define insertR*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l) )
           (cond ((eq? (car l) old) 
                  (cons (car l) 
                        (cons new 
                              (insertR* new old (cdr l)))))
                 (else (cons (car l) 
                             (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l)) 
                      (insertR* new old (cdr l)))))))


(define f 5)


(define occur*
  (lambda (a l) 
    (cond ((null? l) (quote ()))
          ((atom? (car l) )
           (cond ((eq? (car l) a) 
                  (add1 (occur* a (cdr l ))))
                 (else (occur* a (cdr l))) ))
          (else (+ (occur* a (car l))
                   (occur* a (cdr l)))))))                      






(define member*
  (lambda (a l) 
    (cond ((null? l) #f)
          ((atom? (car l))
           (or (eq? (car l) a) 
                (member* a (cdr l))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))                      







(define leftmost*
  (lambda (a l) 
    (cond ((null? l) #f)
          ((atom? (car l) ) (car l)
          (else (leftmost (car l)))))))                      


(define set? 
  (lambda (lat)
    (cond ((null? lat) #t)
          (else 
           (cond 
            ((member? (car lat) (cdr lat)) #f )
            (else (set? (cdr lat)))))))


  (define subset?
    (lambda (set1 set2)
      (cond ((null? set1)  #t)
            ((member? (car set1) set2)
             (subset? (cdr set1) set2) )
            (else #f) )))


;  Chap 9; the ultimate Lambda
  

(define rember-f 
    (lambda (test? a l)
      (cond ((null? l) (quote ()))
            ((test? a (car l)) (cdr l))
            (else (cons (car l) (rember-f test? a (cdr l)))))))


;;; to the function above  we do (rember-f test? a l)


(define rember-f 
    (lambda (test?)
            (lambda (a l)
                (cond ((null? l) (quote ()))
                      ((test? a (car l)) (cdr l) )
                      (else (cons (car l) 
                          ((rember-f test?) a (cdr l))))))))
    
;;  this is a currying version of rember-f , to call this function ,  we do ((rember-f test?) a l)    



















