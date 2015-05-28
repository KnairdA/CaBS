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

(define (perform-add #!optional sources)
  (let ((blob-writer (make-blob-writer
                       (make-file-writer (current-directory))
                       (make-identifier-reader (make-directory-reader
                                                 (current-directory))))))
    (if (not sources)
      (print (blob-writer (read-all (current-input-port))))
      (for-each (lambda (source) (print (blob-writer (read-all source))))
                sources))))

(define (perform-get hashes)
  (let ((blob-reader (make-blob-reader
                       (make-identifier-reader
                         (make-directory-reader (current-directory))))))
    (cond ((null-list? hashes)   (exit 1))
          ((= 1 (length hashes)) (let ((path (blob-reader (first hashes))))
                                   (if (not path)
                                     (exit 1)
                                     (print path))))
          (else (let ((paths (delete #f (map blob-reader hashes))))
                  (for-each print paths)
                  (exit (boolean->exit (= (length paths) (length hashes)))))))))

(define (perform-check hashes)
  (let ((blob-reader (make-blob-reader
                       (make-identifier-reader
                         (make-directory-reader (current-directory))))))
    (if (null-list? hashes)
      (exit 1)
      (exit (boolean->exit (every string? (map blob-reader hashes)))))))

(define (perform-digest)
  (print (get-storage-digest (make-directory-reader (current-directory)))))

(define (perform-validate)
  (let* ((directory-reader  (make-directory-reader (current-directory)))
         (identifier-reader (make-identifier-reader directory-reader))
         (blob-reader       (make-blob-reader       identifier-reader))
         (invalid-nodes     (get-invalid-nodes directory-reader blob-reader)))
    (if (= 0 (length invalid-nodes))
      (exit 0)
      (begin (for-each print invalid-nodes)
             (exit 1)))))
