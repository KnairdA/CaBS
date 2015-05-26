(define (make-kv-printer first-column second-column)
  (lambda (key value)
    (conc (make-string first-column)
          key
          (make-string (- second-column (+ first-column (string-length key))))
          value)))