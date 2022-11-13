;;; vim: set nowrap: -*- truncate-lines: t -*-

(define input '(((55 285 27 323 22 400 20 49 40 336 50 98 36 12 96 294) . (0 2 3 0 0 7 0 0 17 19 20 0 0 42 48 0)) ((70 52 196 17 53 234 35 114 150 29 276 41 31 264 50 78) . (3 3 0 6 6 7 0 0 0 0 26 0 38 44 0 0)) ((35 150 41 77 297 17 133 38 52 352 26 360 66 42 264 18) . (0 0 0 0 7 8 0 11 0 0 0 0 30 0 33 0)) ((116 18 56 24 10 90 20 60 33 16 100 19 23 25 17 70) . (1 0 2 3 4 0 0 0 0 0 14 0 0 0 0 0)) ((60 23 13 6 12 112 17 48 22 18 105 30 26 7 19 9) . (0 0 2 3 3 3 0 6 0 8 12 14 0 16 0 21)) ((25 13 72 30 69 29 100 24 11 9 26 22 15 36 10 54) . (1 2 3 3 3 0 0 0 0 9 10 12 18 20 0 0)) ((132 120 23 27 180 27 130 29 198 28 31 126 168 92 23 26) . (0 0 6 0 9 9 10 0 0 0 0 0 0 0 22 0)) ((10 12 25 99 45 20 16 28 64 20 15 14 8 36 11 16) . (0 2 0 3 5 5 5 5 0 8 9 0 11 11 14 18)) ((12 13 26 17 40 42 18 30 35 13 45 70 30 17 14 15) . (0 2 3 0 4 5 6 0 7 7 0 10 10 13 0 15)) ((27 198 126 37 102 29 132 25 180 144 28 23 210 37 160 41) . (0 5 6 6 6 0 9 11 16 0 18 0 0 0 0 0)) ((144 30 24 180 21 176 26 140 28 27 160 98 33 182 27 200) . (0 7 8 8 10 0 0 12 0 14 14 0 18 0 0 0))))

(define (divisors n)
  (let loop ((k 1))
    (cond ((> (* k k) n) '())
          ((= (mod n k) 0)
           (cons (cons k (/ n k))
                 (loop (+ k 1))))
          (else (loop (+ k 1))))))

(define (pairs xs)
  (let loop ((ys xs))
    (if (null? ys)
        '()
        (let ((ds (divisors (car ys))))
          (append
           (filter (lambda (p)
                     (member (+ (car p) (cdr p)) xs))
                   ds)
           (loop (cdr ys)))))))

(define (score ps)
  (fold-left (lambda (n p) (+ n (cdr p) (- (car p)))) 0 ps))

(define (check ps seq)
  (let ((nums (fold-left (lambda (acc p)
                           (cons (car p) (cons (cdr p) acc)))
                         '() ps)))
    (every (lambda (x y)
             (or (= x y) (= y 0)))
           (sort nums) seq)))

(define (select xs y)
  (cond ((null? xs) #f)
        ((= (car xs) y) (cdr xs))
        (else (let ((rest (select (cdr xs) y)))
                (and rest (cons (car xs) rest))))))

(define (solution puzzle ps)
  (let ((grid (car puzzle))
        (seq (cdr puzzle)))
    (if (null? grid)
        (and (check ps seq) (score ps))
        (any (lambda (p)
               (let ((grid (select grid (+ (car p) (cdr p)))))
                 (and grid
                      (let ((grid (select grid (* (car p) (cdr p)))))
                        (and grid (solution (cons grid seq) (cons p ps)))))))
              
              (pairs grid)))))

(define (answer)
  (fold-left (lambda (n t) (+ n (solution t '()))) 0 input))
