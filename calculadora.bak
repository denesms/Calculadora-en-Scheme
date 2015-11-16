(define frame (new frame%
[label "Calculadora"]
[height 350]))
;APROXIMACION A LA SERIE DE TAYLOR
;variable
(define value   0) ; value of text
(define num1    0) ; value 1
(define num2    0) ; value 2
(define result  0) ; value 2
(define oper    0) ; operation
(define bande   0) ; empty string – used below

;menu bar
(define menu-bar (new menu-bar%
(parent frame)))
(new menu%
(label "&View")
(parent menu-bar))
(new menu%
(label "&Edit")
(parent menu-bar))
(new menu%
(label "&Help")
(parent menu-bar))

;caja de texto
(define panel (new horizontal-panel% [parent frame]))

(define text-field (new text-field%
(label "")
(vert-margin 10)
(horiz-margin 10)
(stretchable-height "2")
(parent panel)
(init-value "")
))

;////////////////////////////////////////////////////////primera fila///////////////////
(define panel (new horizontal-panel% [parent frame] ))

(define msg (new message% [parent panel]
[label "                      "]))

;///////////////////////////////////////////////// logaritmo Natural ///////////////////

(instantiate button% ()
 (label "ln")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (logNat value)
     (send text-field set-value (number->string result)))
     )
  )
))

;////////////////////////

(new button% [parent panel]
[label "("]
[stretchable-height "2"]
(callback (lambda (button event)
(send text-field set-value (string-append "(" (send text-field get-value))        
            )))
)

(new button% [parent panel]
[label ")"]
[stretchable-height "2"]
(callback (lambda (button event)
(send text-field set-value (string-append (send text-field get-value) ")" )        
            )))
)

;///////////////////////////////////////// borrar ////////////////////
(new button% [parent panel]
[label "<-"]
[stretchable-height "2"]
[callback (lambda (button event)
(cond
  ((equal? (send text-field get-value) "") 
     )
  (else  (send text-field set-value (substring (send text-field get-value) 0 (-(string-length (send text-field get-value))1))))
)
)])



;////////////////////////////////////////// CE //////////////////////////
(new button% [parent panel]
[label "ce"]
[stretchable-height "2"]
[callback (lambda (button event)
(set! value 0)
(set! oper  0)
(set! num1  0)
(set! num2  0)
(set! result  0)
(send text-field set-value "")
            )])

;/////////////////////////////////////////////////////  C ////////////////////

(new button% [parent panel]
[label "c"]
[stretchable-height "2"]
[callback (lambda (button event)
(set! value 0)
(set! oper  0)
(set! num1  0)
(set! num2  0)
(set! result  0)
(send text-field set-value "")
            )])

;/////////////////////////////////// poner signo al numero ////////////////////
(new button% [parent panel]
[label "+-"]
[stretchable-height "2"]
[callback (lambda (button event)
(cond 
     ((equal? (send text-field get-value) "")
      )
     (else
      (cond ((eq? (string-ref (send text-field get-value) 0) #\-) 
             (send text-field set-value (substring (send text-field get-value) 1))) 
            
            (else (send text-field set-value (string-append "-" (send text-field get-value))))
            ))
)

)])

;////////////////////////////////////////// Raiz //////////////////////////


(instantiate button% ()
 (label "√")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (raiz value)
     (send text-field set-value (number->string result)))
     )
  )
))

     

;///////////////////////////////////////////////// segunda fila //////////////////////////


(define panel (new horizontal-panel% [parent frame]))
(define msg (new message% [parent panel]
[label "                      "]))

;/////////////////////////////////////////// sin /////////////////

(instantiate button% ()
 (label "sin")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (seno value)
     (send text-field set-value (number->string result)))
     )
  )
))

;/////////////////////////////////////////////////////// X elevado a la 2 //////////////

(instantiate button% ()
 (label "x^2")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (elevado2 value)
     (send text-field set-value (number->string result)))
     )
  )
))

;/////////////////////////////////////////////////////// factorial /////////////

(instantiate button% ()
 (label "n!")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (send text-field set-value (number->string (factorial value))))
     )
  )
))

;///////////////////////////////////// 7 //////////////////////////////////
(new button% [parent panel]
[label "7"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "7"))
            )])

;//////////////////////////////////// 8 ///////////////////////////////////
(new button% [parent panel]
[label "8"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "8"))
            )])

;//////////////////////////////////// 9 ////////////////////////////////////////
(new button% [parent panel]
[label "9"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "9"))
            )])

;////////////////////////////////////////////// division /////////////////////////////////

(instantiate button% ()
 (label "/")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
      (set! value (send text-field get-value))
      (set! oper  4) ;division
      (makeOperation value oper)
      (send text-field set-value (number->string result))
      )
  ))
))

;//////////////////////////////////////////////// modulo //////////////////////////////

(instantiate button% ()
 (label "%")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! oper  5) ;modulo
     (makeOperation value oper)
     (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////////////////////////tercera fila/////////////////////////////
(define panel (new horizontal-panel% [parent frame]))
(define msg (new message% [parent panel]
[label "                      "]))
;///////////////////////////////////////////coseno

(instantiate button% ()
 (label "cos")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (coseno value)
     (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////////////////// x elevado a la y

(instantiate button% ()
 (label "x^y")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! oper  6) ;x ^ y
     (makeOperation value oper)
     (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////////////////////

(define msg (new message% [parent panel]
[label "                      "]))

;///////////////////////////////////////// 4 ////////////////////////////////////
(new button% [parent panel]
[label "4"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "4"))
)])

;///////////////////////////////////// 5 /////////////////////////////////////
(new button% [parent panel]
[label "5"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "5"))
)])

;//////////////////////////////////////// 6 ////////////////////////////////////
(new button% [parent panel]
[label "6"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "6"))
            )])

;;////////////////////////////////////////////////multiplicacion///////////////////////

(instantiate button% ()
 (label "*")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
      (set! value (send text-field get-value))
      (set! oper  3) ;multiplicacion
      (makeOperation value oper)
      (send text-field set-value (number->string result)))
     )
  )
))

;/////////////////////////////////////////////////////// reciproca //////////////

(new button% [parent panel]
[label "1/x"]
[stretchable-height "2"]
[callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
      (set! value (send text-field get-value))
      (set! value (string->number value))
      (funcionReciproca value)
      (send text-field set-value (number->string result)))
  )

)]
)


;///////////////////////////////////////////////////cuarta fila//////////////////////////
(define panel (new horizontal-panel% [parent frame]))
(new button% [parent panel]
[label "π"]
[stretchable-height "2"]
[callback (lambda (button event)
(send text-field set-value (number->string pi))
            )])

;//////////////////////////////////// tangente ///////////////////////

(instantiate button% ()
 (label "tan")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (tangente value)
     (send text-field set-value (number->string result)))
     )
  )
))


;//////////////////////////////////////////////////////// x elevado a la 3///////

(instantiate button% ()
 (label "x^3")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (elevado3 value)
     (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////////////////////////////

(define msg (new message% [parent panel]
[label "                      "]))

;///////////////////////////////////////// 1 ////////////////////////////////////
(new button% [parent panel]
[label "1"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "1"))
            )])

;////////////////////////////////////// 2 ///////////////////////////////////////
(new button% [parent panel]
[label "2"]
[stretchable-height "2"]
[callback (lambda (button event)
(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "2"))
            )])

;/////////////////////////////////////// 3 /////////////////////////////////////
(new button% [parent panel]
[label "3"]
[stretchable-height "2"]
[callback (lambda (button event)

(if (equal? (string->number(send text-field get-value)) 0) 
    (send text-field set-value "")
 )
(send text-field set-value (string-append (send text-field get-value) "3"))
            )])




;///////////////////////////////////////resta//////////////////////////////

(instantiate button% ()
 (label "-")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
      (set! value (send text-field get-value))
      (set! oper  2) ;resta
      (makeOperation value oper)
      (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////////////////quinta fila////////////////////
(define panel (new horizontal-panel% [parent frame]))

;//////////////////////////////////////////////////// Exp /////////////////

(instantiate button% ()
 (label "Exp")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (Exp value)
     (send text-field set-value (number->string result)))
     )
  )
))

;/////////////////////////////////////////////////////// modulo ///////////////


(instantiate button% ()
 (label "Mod")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
      (set! value (send text-field get-value))
      (set! oper  5) ;modulo
      (makeOperation value oper)
      (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////// logaritmo ///////////////////////

(instantiate button% ()
 (label "Log")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (logaritmo value)
     (send text-field set-value (number->string result)))
     )
  )
))

;//////////////////////////////////////////////////////// 10 elevado a la x

(instantiate button% ()
 (label "10^x")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
 (cond 
     ((equal? (send text-field get-value) "")
      )
     (else
     (set! value (send text-field get-value))
     (set! value (string->number value))
     (10elevadoX value)
     (send text-field set-value (number->string result)))
     )
  )
))

;;;;;////////////////////////////////// 0 ///////////////////////////////////
(new button% [parent panel]
[label "0"]
[stretchable-height "2"]
[stretchable-width "2"]
[callback (lambda (button event) 
(send text-field set-value (string-append (send text-field get-value) "0"))
            )])

;//////////////////////////////////////////// punto //////////////////////////////
(new button% [parent panel]
[label "."]
[stretchable-height "2"]
[callback (lambda (button event)
(send text-field set-value (string-append (send text-field get-value) "."))
            )])

;////////////////////////////////////////suma////////////////////////////////////

(instantiate button% ()
 (label "+")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event)
 (cond 
     ((equal? (send text-field get-value) "")
      )
     
     (else
      (set! value (send text-field get-value))
      (set! oper  1) ;suma
      (makeOperation value oper)
      (send text-field set-value (number->string result))
 
      )
     )
  )
))


;///////////////////////////////////////////igual///////////////////////////////////////


(instantiate button% ()
 (label "=")
 (parent panel)
 (stretchable-height "2")
 (callback (lambda (button event) 
     (set! value (send text-field get-value))
     (makeOperation value oper)
     (send text-field set-value (number->string result))
  )
))


;//////////////////////////////////////////Funciones//////////////////////////////

(define (makeOperation value ope)
 
  (cond
    ((= bande 1) 
          (set! bande 0)
          (set! num2  (string->number value))
          (doOperation)
    )
    (else 
           (set! bande 1)
           (set! oper  ope)
           (set! num1  (string->number value))
           (set! result 0)
           ))
)

(define (doOperation)
  (cond
    ((= oper 1) 
          (set! result (+ num1 num2))
    )
    ((= oper 2) 
          (set! result (- num1 num2))
    )
    ((= oper 3) 
          (set! result (* num1 num2))
    )
    ((= oper 4) 
          (set! result (/ num1 num2))
          (set! result (exact->inexact result))
    )
    ((= oper 5) 
          (set! result (modulo num1 num2))
    )
    ((= oper 6) 
          (set! result (expt num1 num2))
    )
   
  )
)

;//////////metodo para seno
(define seno (lambda (x) 
      (set! result (sin x))
))

;//////////metodo para coseno
(define coseno (lambda (x) 
      (set! result (cos x))
))

;//////////metodo para tangente
(define tangente (lambda (x) 
      (set! result (tan x))
))

;//////////metodo para logaritmo
(define logaritmo (lambda (x) 
      (set! result (log x))
))

;//////////metodo para numero elevado a la 2
(define elevado2 (lambda (x) 
      (set! result (expt x 2))
))

;//////////metodo para numero elevado a la 3
(define elevado3 (lambda (x) 
      (set! result (expt x 3))
))

;//////////metodo para 10 elevado al X
(define 10elevadoX (lambda (x) 
      (set! result (expt 10 x))
))

;//////////metodo para elevar la base e
(define Exp (lambda (x) 
      (set! result (exp x))
))

;//////////metodo para logartimo natural
(define logNat (lambda (x) 
      (set! result (/ (log x) (log e)))
))

;////////////metodo factorial
(define factorial
  (lambda(n)
    (if (= n 0)
      1
      (* n (factorial (- n 1)))
    )
  )
)

;///////////// funcion reciproca
(define funcionReciproca (lambda (x) 
      (set! result (/ 1 x))
      (set! result (exact->inexact result))
      )
)

;//////////// Raiz
(define (raiz-iter guess x)
  (if (good-enough? guess x)
      guess
      (raiz-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (square x) (* x x))

(define (raiz x)
   (set! result (raiz-iter 1.0 x))
)



(send frame show #t)