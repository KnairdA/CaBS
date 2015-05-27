(include "src/utility")
(include "src/storage.scm")

(define-record documentation arguments description)
(define-record command       name implementation documentation)

(define (perform-help)
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

(define (perform-get hashes)
  (if (not (null-list? hashes))
    (for-each (lambda (hash) (print (get-blob hash)))
              hashes)
    (exit 1)))

(define (perform-check hashes)
  (if (null-list? hashes)
    (exit 1)
    (exit (boolean->exit (every blob-exists? hashes)))))

(define (perform-digest)
  (print (get-storage-digest)))

(define (perform-validate)
  (let ((invalid-nodes (concatenate (map get-invalid-nodes (get-branches)))))
    (if (= 0 (length invalid-nodes))
      (exit 0)
      (begin (for-each print invalid-nodes)
             (exit 1)))))
