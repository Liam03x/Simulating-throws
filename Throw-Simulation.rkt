;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Throw-Simulation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/universe)
(require 2htdp/image)

; physical constants
(define DEGREES 70)
(define ANGLE_RAD (/ (* DEGREES pi) 180))
(define VELOCITY 70)
(define G 9.81)
(define TIMESPEED 1)

; computes the zero(s)
; ABC formula: [-b +- sqrt(b-4ac)] / 2a
(define a (* -0.5 G))
(define b (* VELOCITY (sin ANGLE_RAD)))
(define -b (- 0 b))
(define ZERO-
  (/ (- -b (sqrt (sqr b))) (* 2 a)))

; graphical constants

; WIDTH|HEIGHT for each positive and negative quadrant -> Full HEIGHT and WIDTH is double as high
(define WIDTH 600) (define -WIDTH (- 0 WIDTH))
(define HEIGHT 400) (define -HEIGHT (- 0 HEIGHT))
; COORDINATE PLANE DEFINITIOn
(define X-AXIS (line (- (* 2 WIDTH) 1) -1 "black")) (define -X-AXIS (line (+ -WIDTH 1) -1 "black"))
(define Y-AXIS (line -1 (- (* 2 HEIGHT 1)) "black")) (define -Y-AXIS (line -1 (+ -HEIGHT 1) "black"))
(define COORDINATE-PLAIN (overlay X-AXIS Y-AXIS))

(define DOT (rectangle 20 20 "solid" "green"))

; functions

; t is a number which represents time
(define (main t)
  (big-bang t
    (on-tick tock)
    (to-draw draw-posn)
    (stop-when last-posn?)))

; t -> t
; adds 1/28 to t each thime the function is called to represent real time in seconds. TIMESPEED speeds up by a factor x
(define (tock t) (+ t (* TIMESPEED (/ 1 28))))

; t -> Posn
; computes the next point of the ball
; y(t) = -1/2gt^2 + (v0*sin(a))*t
; given: t = 3; expected: P(78.79|83.49)
; given: t = 8.67384 (y=0) => expected P(x|0)
(define (next-posn t)
  (make-posn (* VELOCITY (cos ANGLE_RAD) t)
             (+ (* -0.5 G (sqr t)) (* (* VELOCITY (sin ANGLE_RAD)) t))))

; t -> Image
; Coordinate system's (0,0) is at (WIDTH,HEIGHT) -> place every point at computed (x+WIDTH|y+HEIGHT). We subtract half of the image's height to precise it
(define (draw-posn t) (place-image DOT (+ WIDTH (posn-x (next-posn t))) (- (- HEIGHT (posn-y (next-posn t))) (/ (image-height DOT) 2)) COORDINATE-PLAIN))

; t -> Boolean
; returns true [and stops] if the ball has reached the ground. ZERO- is the [-] computed zero
(define (last-posn? t)
  (cond
    [(>=  (posn-x (next-posn t)) (posn-x (next-posn ZERO-))) #t]
    [else #f]))
