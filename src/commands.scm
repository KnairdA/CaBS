(include "src/utility")
(include "src/storage.scm")

(define-record documentation arguments description)
(define-record command       name implementation documentation)

(define (perform-help arguments)
  (let ((printer (make-kv-printer 0 30)))
    (for-each print (map (lambda (command)
                          (let ((documentation (command-documentation command)))
                            (printer (documentation-arguments   documentation)
                                     (documentation-description documentation))))
                         commands))))

(define (perform-add sources)
  (if (not (null-list? sources))
    (for-each (lambda (source) (print (add-blob (read-all source))))
              sources)
    (print (add-blob (read-all (current-input-port))))))
