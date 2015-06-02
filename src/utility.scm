(define (boolean->exit bool)
  (if bool 0 1))

(define (make-map-accessor assoc-list)
  (lambda (key)
    (cdr (assoc key assoc-list))))

(define (make-kv-printer first-column second-column)
  (lambda (key value)
    (conc (make-string first-column)
          key
          (make-string (- second-column (+ first-column (string-length key))))
          value)))
