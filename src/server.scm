(load "kv.scm")

(define (serveRequests conn op)
  (define closed #f)
  (define shutdown #f)

  ((lambda (req)
    (cond ((eqv? "quit" req)
      (set! closed #t)
      (display "SERVER: Connection has been terminated.")
      (newline)
      (socket-send conn "term")
      (socket-close conn))
    ((eqv? "shutdown" req)
      (set! closed #t)
      (set! shutdown #t)
      (display "SERVER: Server is shutting down...")
      (newline)
      (socket-send conn "term")
      (socket-close conn))
    ((not (null? req))
      (display "SERVER: Received data.")
      (newline)
      (socket-send conn (op req)))))
  (stripQuotes (socket-receive conn 256)))

  (if (not closed) (serveRequests conn op) shutdown))


(define (serverShutdown sock)
  (display "SERVER: Server shut down."))

(define (listenForConnections sock op)
  (socket-listen sock)

  ((lambda (conn)
    (display "SERVER: New connection accepted.")
    (newline)

    (if (not (serveRequests conn op)) (listenForConnections sock op)
      (serverShutdown sock)))
  (socket-accept sock)))


(define (startServer op)
  (socket-create sock)
  (display "-- SchemeKV Server --")
  (newline)
  (newline)
  (display "Select a server port (leave empty for 4000): ")
  ((lambda (port) (socket-bind sock "0.0.0.0" port))
    ((lambda (p) (if (empty? p) 4000 (string->number p))) (read-raw)))

  (display "SERVER: Server started.")
  (newline)

  (display "SERVER: Loaded ")
  (display (initDB))
  (display " items from disk.")
  (newline)

  (listenForConnections sock op))

(startServer sendToDB)
