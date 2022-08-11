#lang scheme
; 2017400189

; Solver function
(define TENTS-SOLUTION (lambda (list1)
                         
                            (INTERMEDIATE (car list1) (car (cdr list1)) (car (cddr list1)) '() (length (car list1)) (length (car (cdr list1))))
                            
                         )
 )                       

; with using POS1 function it gets the possible tent coordinates (possibles)
; and gives it to SOLUTION1 function for it to solve the problem recursively.
(define INTERMEDIATE (lambda (rows columns trees tents maxRowNum maxColumnNum)
                       (if (or(DUMB rows maxColumnNum)(DUMB columns maxRowNum)) #f
                           (if (and (null? trees) (or (not(CHECK columns)) (not(CHECK rows)))) #f
                               (SOLUTION1 rows columns tents (length trees) (POS1 trees maxRowNum maxColumnNum (findZERO rows 1) (findZERO columns 1))))
                               )
                       )   
                        
)

; SOLUTION1 is named SOLUTION1, because it is named after
; another failed one "SOLUTION".

(define SOLUTION1 (lambda (rows columns tents N possibles)

              
                    (if(or (member -1 rows) (member -1 columns)) #f
                            (if(and (not (null? tents))(ADJACENT-WITH-LIST (car tents) (cdr tents))) #f
                               (if (eq? N 0) tents
                                   (if (null? possibles) '()
                                   
                                      
                                                   (if(eq? (length (car possibles)) 1)
                                                            (SOLUTION1 (MINUS-ONE-NTH rows (caaar possibles)) (MINUS-ONE-NTH columns (cadaar possibles)) (cons (caar possibles) tents) (- N 1) (cdr possibles)) 
                                                        (if(eq? (length (car possibles)) 2)
                                                           (or (SOLUTION1 (MINUS-ONE-NTH rows (caaar possibles)) (MINUS-ONE-NTH columns (cadaar possibles)) (cons (caar possibles) tents) (- N 1) (cdr possibles))
                                                               (SOLUTION1 (MINUS-ONE-NTH rows (caadar possibles)) (MINUS-ONE-NTH columns (cadr (cadar possibles))) (cons (cadar possibles) tents) (- N 1) (cdr possibles)))
                                                           (if (eq? (length (car possibles)) 3)
                                                               (or (SOLUTION1 (MINUS-ONE-NTH rows (caaar possibles)) (MINUS-ONE-NTH columns (cadaar possibles)) (cons (caar possibles) tents) (- N 1) (cdr possibles))
                                                                   (SOLUTION1 (MINUS-ONE-NTH rows (caadar possibles)) (MINUS-ONE-NTH columns (cadr (cadar possibles))) (cons (cadar possibles) tents) (- N 1) (cdr possibles))
                                                                   (SOLUTION1 (MINUS-ONE-NTH rows (car (caddar possibles))) (MINUS-ONE-NTH columns (cadr (caddar possibles))) (cons (caddar possibles) tents) (- N 1) (cdr possibles)))
                                                               (if (eq? (length (car possibles)) 4)
                                                                   (or (SOLUTION1 (MINUS-ONE-NTH rows (caaar possibles)) (MINUS-ONE-NTH columns (cadaar possibles)) (cons (caar possibles) tents) (- N 1) (cdr possibles))
                                                                       (SOLUTION1 (MINUS-ONE-NTH rows (caadar possibles)) (MINUS-ONE-NTH columns (cadr (cadar possibles))) (cons (cadar possibles) tents) (- N 1) (cdr possibles))
                                                                       (SOLUTION1 (MINUS-ONE-NTH rows (car (caddar possibles))) (MINUS-ONE-NTH columns (cadr (caddar possibles))) (cons (caddar possibles) tents) (- N 1) (cdr possibles))
                                                                       (SOLUTION1 (MINUS-ONE-NTH rows (caar (cdddar possibles))) (MINUS-ONE-NTH columns (cadar (cdddar possibles))) (cons (car (cdddar possibles)) tents) (- N 1) (cdr possibles))
                                                                       )
                                                                   #f
                                                                   )
                                                             )
                                                         )
                                                     )
                                  )
                             )
                       )
                   )
             )
)

(define CHECK (lambda (list1)
                (if (null? list1) #t (if(not(eq? (car list1) 0)) #f (CHECK (cdr list1))))
                )
  )
; used in POS1. what it does is basically checking the neighbor list of a tree
; and returns the ones that are ok for a tent to be placed.
(define POS2 (lambda (list1 maxRowNum maxColumnNum trees impos1 impos2)
              (if(null? list1) '()  (if (or ( ZERO? (car list1) impos1 impos2) (not (INTHEBOARD? (car list1) maxRowNum maxColumnNum)) (member (car list1) trees)) (append '() (POS2 (cdr list1) maxRowNum maxColumnNum trees impos1 impos2)) (cons (car list1) (POS2 (cdr list1) maxRowNum maxColumnNum trees impos1 impos2)) ))
               )
  )

; POS1 is used in the INTERMEDIATE FUNCTION.
; it basically finds the coordinates that are
; impossible for a tent to be placed, and get rid of them.
; after that it returns the coordinates that are available.
(define POS1 (lambda (trees maxRowNum maxColumnNum impos1 impos2)
               (if(null? trees)
                  '()
                  (cons (POS2 (NEIGHBOR-LIST (car trees)) maxRowNum maxColumnNum trees impos1 impos2) (POS1 (cdr trees) maxRowNum maxColumnNum impos1 impos2))
                  )
              )
  )

; this method is used to detect if a possible coordinate for a tent
; is not permitted by the number of tents allowed in that row or column
; ex. (ZERO? '(3 4) '(1 3 7) '(2 8)) ------> true
; ex. (ZERO? '(3 4) '(1 5 7) '(2 8)) ------> false
(define ZERO? (lambda (list1 list2 list3)
                (if(null? list1) #f (if(or (member (car list1) list2) (member (cadr list1) list3)) #t #f))
                )
  )

; findZERO function takes a list (in this program rows and columns) and finds the
; 0 values and puts indexes of 0 values into a list.
; also 1 has to be given as n.
; ex. (findZERO '(1 2 0 3 4 0 5) '1) ----> (3 6)
(define findZERO (lambda (list1 n)
                   (if(null? list1) '() (if(= (car list1) 0) (cons n (findZERO (cdr list1) (+ n 1))) (findZERO (cdr list1) (+ n 1))))
                   )
 )




; checks if a coordinate is in the board. if not returns false
(define INTHEBOARD? (lambda (list1 max1 max2)
                      (if(< (car list1) 1) #f
                         (if(> (car list1) max1) #f
                            (if(< (car (cdr list1)) 1) #f
                               (if(> (car (cdr list1)) max2) #f
                                  #t
                               )
                            )
                      )
                      )
                      )
  )


; checks if the given puzzle is dumb meaning there cannot be any possible solution
; because in a row or a column there are more trees then half of that row or a column. 
(define DUMB (lambda (list x)
                (if(null? list) #f (if(> (car list) (/ x 2)) #T (DUMB (cdr list) x)))
                )
  )


; substracts one from an index (defined as n) of a list
(define MINUS-ONE-NTH (lambda (list1 n)
                        (if(< n 1) #f
                           (if(null? list1)
                                     '()
                                     (if(eq? n 1)
                                        (cons (- (car list1) 1) (cdr list1))
                                            (cons (car list1) ( MINUS-ONE-NTH (cdr list1) (- n 1)))
                                       )
                               )
                           )
                                  
                      )
  )


; Helper functions
;------------------

(define REPLACE-NTH (lambda (list1 n x)
                               (if(null? list1)
                                     '()
                                     (if
                                       (eq? n 1) (cons x (cdr list1))
                                            (cons (car list1) (REPLACE-NTH (cdr list1) (- n 1) x))
                                       )
                               )
                      )
  )


(define RETURN-FIRST-NOT-FALSE (lambda (func  list1)
                                     (if (null? list1)
                                         #f
                                         (
                                          if (not (func (car list1))) (RETURN-FIRST-NOT-FALSE func (cdr list1)) (func (car list1))
                                          )
                                      )
                                )
)

(define ADJACENT(lambda (list1 list2)
                (if
                    (or
                    (> (- (car list1) (car list2)) 1)
                    (> (- (car (cdr list1)) (car (cdr list2)) ) 1)
                    (< (- (car list1) (car list2)) -1)
                    (< (- (car (cdr list1)) (car (cdr list2))) -1)
                    )
                    #f
                    #t
                    )   
                 )
  )
  
  

(define ADJACENT-WITH-LIST (lambda (list1 list2)
                                        (if (null? list2) #f
                                                 (if (ADJACENT list1 (car list2)) #t (ADJACENT-WITH-LIST list1 (cdr list2))) 
                                         )
                           ) 
)
                                                    


(define NEIGHBOR-LIST (lambda (list1)
                        (if(null? list1) '()
                            (cons (cons (+ (car list1) 1) (cons (car(cdr list1)) '()))  ( cons (cons (- (car list1) 1) (cons (car(cdr list1)) '()))    (cons (cons (car list1) (cons (+ (car(cdr list1)) 1) '()))    (cons (cons (car list1) (cons (- (car(cdr list1)) 1) '())) '()))))
                      ))
)





