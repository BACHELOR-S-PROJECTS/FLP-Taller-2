#lang eopl

(define Bignum
  (lambda (numero-to-convert)
    ;Bignum-aux: funcion que halla la representacion Bignum para una numero, necesita la minima potencia que va a estar al
    ;lado izquierdo (para estos casos siempre sera 0)y un N (base) que en este caso es 16, si se cambia el 16 hay que cambiar mas cosas para que siga funcionando.
    (define Bignum-aux
      (lambda (numero-to-convert pot-actual)
        (if (eq? numero-to-convert 0)
            (if (eq? pot-actual -1)
                '()
                (append (Bignum-aux 0 (- pot-actual 1)) '(0)))
            (let*
                ([N 16]
                 [N-elevado (expt N pot-actual)]
                 [q (floor (/ numero-to-convert N-elevado))])
              (append (Bignum-aux (- numero-to-convert (* q N-elevado)) (- pot-actual 1)) (list q))
              )
            )
        ))
    (let
        ([N 16])
      (Bignum-aux numero-to-convert (buscar-minima-mayor-pot numero-to-convert 0 N))
      )
    ))
;(Bignum 33)
;'(1 2)
;(Bignum 4419)
;(3 4 1 1)
;(Bignum 258)
;(2 0 1)
;(Bignum 3488135)
;(7 8 9 3 5 3)
;(Bignum 256)
;(0 0 1)
;(Bignum 255)
;(15 15)

(define buscar-minima-mayor-pot
  (lambda (numero potencia-test N)
    (if (or (> numero (expt N potencia-test)) (eq? numero (expt N potencia-test)))
        (buscar-minima-mayor-pot numero (+ 1 potencia-test) N)
        (- potencia-test 1)
        )))
;(buscar-minima-mayor-pot 0 0 16)
;-1
;(buscar-minima-mayor-pot 1 0 16)
;0
;(buscar-minima-mayor-pot 16 0 16)
;1
;(buscar-minima-mayor-pot 17 0 16)
;1
;(buscar-minima-mayor-pot 4419 0 16)
;3

(define zero
  (lambda () '()))
;(zero)
;()
;(zero)
;()

(define is-zero?
  (lambda (lista)
    (null? lista)))
;(is-zero? '())
;#t
;(is-zero? '(1))
;#f

(define successor
  (lambda (lista)
    (if (eq? lista '())
        '(1)
        (if (eq? (car lista) 15)
            (if (eq? (length lista) 1)
                '(0 1);aumenta uno al final
                (append '(0) (successor (cdr lista))))
            (append (list (+ 1 (car lista))) (cdr lista))))))
;(successor '(0 0 1))
;256 -> 257
;'(1 0 1)
;(successor '(15 15))
;255 -> 256
;'(0 0 1)
;(successor '(7 8 9 3 5 3))
;3488135 -> 3488136
;'(8 8 9 3 5 3)
;(successor '(3 4 1 1))
;4419 -> 4420
;'(4 4 1 1)

(define predecessor
  (lambda (lista)
    (if (eq? (car lista) 0)
        (append '(15) (predecessor (cdr lista)))
        (if (or (equal? lista (list 1)) (equal? lista (list)))
            '()
            (append (list (- (car lista) 1)) (cdr lista))))
    ))
;(predecessor '(1))
;'()
;(predecessor '(0 1))
;'(15)
;(predecessor '(4 4 1 1))
;(3 4 1 1)
;(predecessor '(8 8 9 3 5 3))
;(7 8 9 3 5 3)
;(predecessor '(0 0 1))
;(15 15)
;(predecessor '(1 0 1))
;(0 0 1)

;suma para Bignum datatype
;solo tengo en cuenta dos sumandos

(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))
;(suma '(15 15) '(0 0 1))
;(15 15 1)
;(suma '(15 15) '(1))
;(0 0 1)
;(suma '(15 15) '())
;(15 15)

(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))
;(resta '(0 0 1) '(15 15))
;(1)
;(resta '(15 15) '())
;(15 15)
;(resta '(15 15) '(1))
;(14 15)

(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))
;(multiplicacion '(1) '(0 1))
;(0 1)
;(multiplicacion '(0 1) '(1))
;(0 1)
;(multiplicacion '(0 1) '(0 0 1))
;(0 0 0 1)
;(multiplicacion '(0 0 1) '(0 1))
;(0 0 0 1)
    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor '())
        (multiplicacion (potencia x (predecessor y)) x))))
;(potencia '(2) '(8))
;(0 0 1)
;(potencia '(3) '(8))
;(1 10 9 1)

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor '())
        (multiplicacion n (factorial (predecessor n))))))
;(factorial '(3))
;(6)
;(factorial '(8))
;(0 8 13 9)
;(factorial '(10))
;(0 0 15 5 7 3)
