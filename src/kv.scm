(load "stdlib.scm")

(hashmap-make db)

(define (fill fd n)
  ((lambda (line) (if (empty? line) n (begin (eval (unstringify line)) (fill fd (+ n 1)))))
    (read-raw fd)))

(define (setupDB fd)
  (fill fd 0))

(define (initDB)
  ((lambda (fd)
    (define res (setupDB fd))
    (close-input-port fd)
    res)
  (open-input-file ".schemeKV")))

(define (setVal key value)	(hashmap-insert db (list key value)))
(define (getVal key)		(hashmap-lookup db key))
(define (deleteVal key)		(hashmap-delete db key))

(define (set key value) (setVal key value) value)

(define (get key)
  ((lambda (v)
    (if (eqv? v #f) "Key not found in database." v))
  (getVal key)))

(define (del key)       (deleteVal key) (stringify key))
(define (ex key)        (eval (getVal key)))
(define (ev expr)       (eval expr))

(define (pers key)
  ((lambda (value)
  (if (eqv? value #f) "Key not found in database."
    ((lambda (v fd)
      ((lambda (msg) (display msg fd))
        (string-append "(set '" (stringify key) " (quote " v "))"))
      (display "\n" fd)
      (close-append-port fd)
      "Data saved.")
    (stringify value)
    (open-append-file ".schemeKV"))))
  (getVal key)))

(define (sendToDB cmd)
  (stringify (eval (unstringify cmd))))
