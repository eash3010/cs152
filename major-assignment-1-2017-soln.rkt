#lang racket


(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)

(define (draw curve)
  (plot (parametric
         (lambda (t) (vector
                      (x-of (curve t))
                      (y-of (curve t))))
         0 1 #:width 1 #:samples 70000
         #:x-min -5 #:x-max 5
         #:y-min -5 #:y-max 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))
(define (y-of point)
  (point 1))

(define (identity x) x)
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))





(define (unit-circle)
  (lambda (t)
  (make-point (sin (* 2 pi t))
              (cos (* 2 pi t)))))

(define (unit-semi-circle)
  (lambda (t)
  (make-point (sin (* pi t))
              (cos (* pi t)))))

(define (unit-line-at y)
  (lambda (t) (make-point t y)))



(define (unit-line) (unit-line-at 0))


(define (vertical-line p l)
  (lambda (t) (make-point (x-of p) (+ (y-of p) (* t l)))))

;(draw (vertical-line (make-point 0.5 0.5) 2))


(define (rotate-pi/2 curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (- (y-of ct))
       (x-of ct)))))

(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (- (x-of ct))
       (y-of ct)))))

;(draw (reflect-through-y-axis (vertical-line (make-point 0.5 0.5) 2)))
;(draw (reflect-through-y-axis (unit-circle)))

(define (translate x y curve) 
    (lambda (t)
      (let ((ct (curve t)))
        (make-point
         (+ x (x-of ct))
         (+ y (y-of ct))))))

;(draw (translate 0.5 0.5  (reflect-through-y-axis (vertical-line (make-point 0.5 0.5) 2))))
;(draw (translate 0.5 -0.5 (reflect-through-y-axis (unit-circle))))

(define (scale x y curve) 
    (lambda (t)
      (let ((ct (curve t)))
        (make-point
         (* x (x-of ct))
         (* y (y-of ct))))))

;(draw (scale 2 0.5 (reflect-through-y-axis (vertical-line (make-point 0.5 0.5) 2))))
;(draw (scale 0.2 -0.5 (reflect-through-y-axis (unit-circle))))


(define (rotate-around-origin theta curve) 
    (lambda (t)
      (let* ([ct (curve t)]
             [x (x-of ct)]
             [y (y-of ct)])
        (make-point 
         (- (* (cos theta) x) (* (sin theta) y)) 
         (+ (* (cos theta) y) (* (sin theta) x))))))

;(draw (rotate-around-origin (- (/ pi 4)) (scale 2 0.5 (reflect-through-y-axis (vertical-line (make-point 0.5 0.5) 2)))))
;(draw (rotate-around-origin (/ pi 4) (scale 0.2 -0.5 (reflect-through-y-axis (unit-circle)))))

(define (put-in-standard-position curve)
  (define (begin-x curve) (x-of (curve 0)))
  (define (begin-y curve) (y-of (curve 0)))
  (define (end-x curve) (x-of (curve 1)))
  (define (end-y curve) (y-of (curve 1)))
  (let* ([curve1 (translate (- (begin-x curve))
                            (- (begin-y curve))
                            curve)]
         [curve2 (rotate-around-origin (- (atan (/ (end-y curve1)
                                                   (end-x curve1)))) curve1)]
         [scale-factor  (/ 1 (sqrt
                              (+ (sqr (- (begin-x curve2) (end-x curve2)))
                                 (sqr (- (begin-y curve2) (end-y curve2))))))]
         [curve3 (scale scale-factor scale-factor curve2)])
    curve3))
    


;(draw (put-in-standard-position (rotate-around-origin (- (/ pi 4)) (scale 2 0.5 (reflect-through-y-axis (vertical-line (make-point 0.5 0.5) 2))))))
;(draw (put-in-standard-position (scale 0.2 -0.5 (reflect-through-y-axis (unit-circle)))))


(define (connect-rigidly curve1 curve2)
  (lambda (t)
    (if (< t (/ 1 2))
        (curve1 (* 2 t))
        (curve2 (- (* 2 t) 1)))))

;(draw (connect-rigidly
;       (put-in-standard-position
;        (rotate-around-origin (- (/ pi 4))
;                              (scale 2 0.5
;                                     (reflect-through-y-axis
;                                      (vertical-line
;                                       (make-point 0.5 0.5) 2)))))
;       (scale 0.2 -0.5 (reflect-through-y-axis
;                        (unit-circle)))))

(define (connect-ends curve1 curve2)
  (let* ([begin-x-2 (x-of (curve2 0))]
         [begin-y-2 (y-of (curve2 0))]
         [end-x-1 (x-of (curve1 1))]
         [end-y-1 (y-of (curve1 1))])
    (connect-rigidly curve1 (translate (- end-x-1 begin-x-2)
                                        (- end-y-1 begin-y-2)
                             curve2))))
;(draw (connect-ends
;       (put-in-standard-position
;        (rotate-around-origin (- (/ pi 4))
;                              (scale 2 0.5
;                                     (reflect-through-y-axis
;                                      (vertical-line
;                                       (make-point 0.5 0.5) 2)))))
;       (scale 0.2 -0.5 (reflect-through-y-axis
;                        (unit-circle)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gosper-curve level)
  ((repeated gosper-step level) (unit-line)))

(define (gosper-step curve)
  (let ([scaled-curve (scale (/ (sqrt 2) 2) (/ (sqrt 2) 2) curve)])
    (connect-rigidly (rotate-around-origin (/ pi 4) scaled-curve)
                     (translate .5 .5
                                (rotate-around-origin (/ (- pi) 4) scaled-curve)))))

;(draw (gosper-curve 1))
;(draw (gosper-curve 4))
;(draw (gosper-curve 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gosper-general curve calc-angle l)
  (define (gosper-general-step pair)
    (let* ([curve (car pair)]
           [level (cdr pair)]
           [angle (calc-angle level)]
           [scaled-curve (scale (/ 0.5 (cos angle)) (/ 0.5 (cos angle)) curve)])
      (cons (connect-rigidly
             (rotate-around-origin angle scaled-curve)
             (translate .5 (* 0.5 (tan angle))
                        (rotate-around-origin (- angle) scaled-curve)))
            (- level 1))))
  (car ((repeated gosper-general-step l)
        (cons (put-in-standard-position curve) 0))))

(define (calc-angle level)
    (if (= level 0) (/ pi 4)
        (- (/ pi 4) (/ (/ pi 4) (expt level 2)))))

;(draw (gosper-general (put-in-standard-position (unit-semi-circle)) calc-angle 8))
;(draw (gosper-general (unit-line) calc-angle 8))
;(draw (gosper-general (put-in-standard-position (unit-semi-circle)) calc-angle 32))
;(draw (gosper-general (unit-line) calc-angle 32))


;(draw (gosper-general (sierpinski-curve 4) calc-angle 32))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kochize curve)
  (connect-ends (scale (/ 1 3) (/ 1 3) curve)
                (connect-ends (connect-ends (rotate-around-origin (/ 3.1415 3)
                                                                   (scale (/ 1 3) (/ 1 3) curve)) 
                                            (rotate-around-origin (- (/ 3.1415 3)) 
                                                                    (scale (/ 1 3) (/ 1 3) curve)))
                              (scale (/ 1 3) (/ 1 3) curve))))

(define (koch-curve level)
  (let* ; start with only one side kochized 
      ([one-side ((repeated kochize level) (unit-line))]
       ; flip it so that the curves start at x = 1 and ends at x=0
       [flip (translate 1 0 (rotate-around-origin pi one-side))]
       [upslope (rotate-around-origin (/ pi 3) one-side)]
       [downslope (rotate-around-origin (- (/ pi 3)) one-side)])
    (connect-ends (connect-ends flip upslope) downslope)))

;(draw (koch-curve  2))
;(draw (koch-curve  4))
;(draw (koch-curve  8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define point (lambda (t) (make-point 1.0 -0.5)))

(define (unit-triangle)
  (let* ([l1 (unit-line)]
         [l2 (rotate-around-origin (/(* 4 pi) 3) (translate -1 0 l1))]
         [l3 (rotate-around-origin (/(* 5 pi) 3) (translate -0.5 (/ (sqrt 3) 2) l1))]
         [l4 (connect-rigidly l2 l1)]
         [l5 (connect-rigidly l4 l3)])
    l5))
         

(define (sierpinski-ize curve)
  (let* ([scaled-curve (scale .5 .5 curve)]
         [first-bottom scaled-curve]
         [second-bottom (translate .5 0 scaled-curve)]
         [top (translate .25 (/ (sqrt 3) 4)  scaled-curve)]
         [top-1b (connect-rigidly top first-bottom)]
         [top-1b-2b (connect-rigidly top-1b second-bottom )])
    top-1b-2b))

(define (sierpinski-curve level) 
  ((repeated sierpinski-ize level) (unit-triangle)))

;(draw (sierpinski-curve 2))
;(draw (sierpinski-curve 4)) 
;(draw (sierpinski-curve 8))
;(draw (sierpinski-curve 32))
;(draw (gosper-general (sierpinski-curve 4) calc-angle 32))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (draw-in-file  curve out-file out-kind)
;  (plot (parametric
;         (lambda (t) (vector  (x-of (curve t)) 
;                              (y-of (curve t))))
;         0 1 #:width 1 #:samples 20000 
;         #:x-min -0.5 #:x-max 1.5
;         #:y-min -0.5 #:y-max 1.5)
;         #:out-file out-file	 
;         #:out-kind out-kind))
;
;(draw-in-file (koch-curve 0) "koch-level-0.pdf" 'pdf)
;(draw-in-file (koch-curve 16) "koch-level-1.pdf" 'pdf)
;(draw-in-file (koch-curve 2) "koch-level-2.pdf" 'pdf)
;(draw-in-file (koch-curve 3) "koch-level-3.pdf" 'pdf)
;(draw-in-file (koch-curve 4) "koch-level-4.pdf" 'pdf)
;(draw-in-file (koch-curve 8) "koch-level-8.pdf" 'pdf)
;
;
;(draw-in-file (sierpinski-curve 0) "sierpinski-curve-0.pdf" 'pdf)
;(draw-in-file (sierpinski-curve 1) "sierpinski-curve-1.pdf" 'pdf)
;(draw-in-file (sierpinski-curve 2) "sierpinski-curve-2.pdf" 'pdf)
;(draw-in-file (sierpinski-curve 4) "sierpinski-curve-3.pdf" 'pdf)
;(draw-in-file (sierpinski-curve 4) "sierpinski-curve-4.pdf" 'pdf)
;(draw-in-file (sierpinski-curve 8) "sierpinski-curve-8.pdf" 'pdf)


