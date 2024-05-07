(define (abs x)
    (cond ((< x 0) (- x))
    (else x)))

(define (good-enough? oldguess newguess)
    (if (= oldguess 0) #f
     (< (abs (/ (- newguess oldguess) oldguess)) .001)))

(define (square x) (* x x))

(define (avg3 x y z) (/ (+ x y z) 3))

(define (improve guess x)
    (avg3 (/ x (square guess)) guess guess))

(define (cubeiter oldguess guess x)
    (if (= x 0) x
        (if (good-enough? oldguess guess) guess
            (cubeiter guess (improve guess x) x))))

(define (cubert x) (cubeiter 0 1.0 x))

(display (cubert 100))
