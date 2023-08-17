(load "stdlib.scm")

(define conn '())

(define closed #f)

(define (sendCmd cmd)
  (socket-send conn cmd))
  
(define (loopCLI sock)
  (display "Scheme-KV> ")
  ((lambda (cmd) (if (empty? cmd) (loopCLI sock) (sendCmd cmd)))
    (read-raw))
  (newline)
  ((lambda (rec)
    (if (eqv? rec 'term)
      (set! closed #t)
      (write rec))) (unstringify (stripQuotes (socket-receive conn 256))))
  (newline)
  (newline)
  (if (not closed) (loopCLI sock) (begin (display "Connection terminated.") (newline))))

(define (connectTo host port)
  (display "CLIENT: Connnecting to ")

  (socket-create sock)
  (socket-connect sock 
    ((lambda (resHost) (display resHost) (display ":") resHost)
     ((lambda (h) (if (empty? h) "127.0.0.1" h)) host))
    ((lambda (p) (if (empty? p) (begin (display 4000) 4000) (begin (display p) (string->number p)))) port))
  (display " . . .")
  (newline)

  (set! conn sock)

  (display "CLIENT: Connected to server.")
  (newline)

  (loopCLI sock))

(define (startClient)
  (display "-- SchemeKV Client --")
  (newline)
  (newline)

  ((lambda (h p) (connectTo h p))
  (begin (display "Server address (leave empty for this machine): ")
	 (read-raw))
  (begin (display "Server port (leave empty for default): ")
         (read-raw))))

(startClient)
