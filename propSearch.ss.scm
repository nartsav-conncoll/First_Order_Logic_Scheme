(define bindings '())

(define rules
  '(
  ;A&B are temp variables for bindings
  ;
	(((goal A)(visited A)) (finished))
	(((visited A)(adj A B)((not visited) B) ((not obstacle) B))(visited B))
	 
  ))

(define facts
  '((goal '(2 2)) (visited '00) (obstacle '11) (obstacle '21)))
  
(define adjacent  
  (lambda (node)
    (let ((x (car node))
          (y (cadr node)))
      (display x)
      (display y)
      (list 
        (list 'adj (+ x 1) y)
        (list 'adj (- x 1) y)
        (list 'adj x (+ y 1))
        (list 'adj x (- y 1))
      )
    )
  )
)
(define unify (lambda (pred1 pred2)
  (cond [(equal? pred1 pred2) bindings]
        [(var? pred1) (add-bindings pred1 pred2)]
        [(var? pred2) (add-bindings pred2 pred1)]
        
        [else #f]
                )
  ))




(define add-bindings (lambda (var val)
  (let ((existing-binding (member? var bindings)))
    (if existing-binding
        (unify (binding-value existing-binding) val bindings)
        (cons (make-binding var val) bindings)))))




  

(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))
      
(define ModusPonens2 
  (lambda (b a)
    (if (null? b)
        a
    ;else
        (let ((fact1 (car b)))
          (if (list? fact1)
              (let ((fact1n (cadr fact1)))
                (if (member fact1n facts)
                    '()
                ;else
                    (ModusPonens2 (cdr b) a)))
           ;else
              (if (member fact1 facts)
                  (ModusPonens2 (cdr b) a)
              ;else
                  '()))))))

(define search
  (lambda (count)
    (cond
      ((member 'finished facts)
          (display "goal found")
          (newline))
      ((<= count 0)
        (display "not found")
        (newline))
      (else
        (let* ((firstRule (car rules))
               (remainingRules (append (cdr rules) (list firstRule)))
               (newFact (ModusPonens firstRule)))
          (cond 
            ((null? newFact) 
                (set! rules remainingRules)
                (search (- count 1)))
            (else
                (set! rules remainingRules)
                (set! facts (append newFact facts))
                (search (- count 1)))))))))

